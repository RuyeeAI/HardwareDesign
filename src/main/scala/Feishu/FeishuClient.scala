package Feishu

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Duration
import java.util.concurrent.atomic.AtomicReference
import scala.util.Try

/**
 * 飞书开放平台 API 客户端（基础库，无第三方 HTTP 依赖，使用 JDK 内置 HttpClient）。
 *
 * 提供：
 *  - tenant_access_token 获取与缓存（过期前自动刷新）
 *  - 发送文本消息 / 富文本 post 消息 / interactive 卡片消息
 *  - 上传文件（im/v1/files），返回 file_key 供发送文件消息复用
 *  - 通用 `call` 方法（JSON in / JSON out），便于扩展其它 API
 *
 * 用法：
 * {{{
 *   val client = FeishuClient(FeishuConfig.load())
 *   val resp = client.sendText("oc_xxx", "hello")
 * }}}
 *
 * 所有方法返回飞书原始响应 JSON 字符串；`respOk` 可校验 code==0。
 */
class FeishuClient(cfg: FeishuConfig) {
  import FeishuClient._

  private val http = HttpClient.newBuilder()
    .connectTimeout(Duration.ofSeconds(10))
    .build()

  // ---------------- tenant_access_token 缓存 ----------------
  private case class Token(token: String, expireAtMillis: Long)
  private val tokenRef = new AtomicReference[Token](null)

  /** 获取（并缓存）tenant_access_token；过期前 300s 自动刷新 */
  def tenantAccessToken(): String = {
    val now = System.currentTimeMillis()
    val cur = tokenRef.get()
    if (cur != null && cur.expireAtMillis - 300_000 > now) cur.token
    else {
      val json =
        s"""{"app_id":"${cfg.appId}","app_secret":"${cfg.appSecret}"}"""
      val resp = call("/open-apis/auth/v3/tenant_access_token/internal", json, needAuth = false)
      if (!respOk(resp)) sys.error(s"tenant_access_token failed: $resp")
      val token = extractString(resp, "tenant_access_token")
        .getOrElse(sys.error(s"tenant_access_token missing in: $resp"))
      val expire = extractLong(resp, "expire").getOrElse(7200L)
      tokenRef.set(Token(token, now + expire * 1000L))
      token
    }
  }

  // ---------------- 通用请求 ----------------
  /** 发送 JSON 请求，返回响应体字符串（POST）；needAuth=true 时带 Authorization */
  def call(path: String, json: String, needAuth: Boolean = true): String = {
    val url = cfg.baseUrl + path
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(Duration.ofSeconds(30))
      .header("Content-Type", "application/json; charset=utf-8")
    if (needAuth) builder.header("Authorization", s"Bearer ${tenantAccessToken()}")
    val req = builder.POST(HttpRequest.BodyPublishers.ofString(json, StandardCharsets.UTF_8)).build()
    val resp = http.send(req, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    resp.body()
  }

  /** GET 请求（可选带鉴权），返回响应体字符串 */
  def callGet(path: String, needAuth: Boolean = true): String = {
    val url = cfg.baseUrl + path
    val builder = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(Duration.ofSeconds(30))
    if (needAuth) builder.header("Authorization", s"Bearer ${tenantAccessToken()}")
    val req = builder.GET().build()
    val resp = http.send(req, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    resp.body()
  }

  // ---------------- 消息发送 ----------------
  /** 发送文本消息；receiveId 缺省用配置的 chatId */
  def sendText(receiveId: String = cfg.chatId.getOrElse(""), text: String): String =
    sendMsg(receiveId, "text", s"""{"text":${jsonStr(text)}}""")

  /** 发送富文本 post 消息（多段，支持中文、链接等）；title 为 post 标题 */
  def sendPost(receiveId: String = cfg.chatId.getOrElse(""),
               title: String,
               lines: Seq[Seq[(String, String)]]): String = {
    // lines: 每行一组 (文本, 样式: text | a | ...)；此处按 text 处理
    val content = lines.map { row =>
      val segs = row.map { case (t, style) =>
        if (style == "a") s"""{"tag":"a","text":${jsonStr(t)},"href":"${t}"}"""
        else s"""{"tag":"text","text":${jsonStr(t)}}"""
      }.mkString("[", ",", "]")
      segs
    }.mkString("[", ",", "]")
    sendMsg(receiveId, "post", s"""{"zh_cn":{"title":${jsonStr(title)},"content":$content}}""")
  }

  /** 发送 interactive 卡片消息（markdown 正文） */
  def sendCard(receiveId: String = cfg.chatId.getOrElse(""),
               headerTitle: String,
               markdown: String): String = {
    val content =
      s"""{"config":{"wide_screen_mode":true},"header":{"title":{"tag":"plain_text","content":${jsonStr(headerTitle)}}},"elements":[{"tag":"markdown","content":${jsonStr(markdown)}}]}"""
    sendMsg(receiveId, "interactive", content)
  }

  /** 发送文件消息（需先 uploadFile 得到 file_key） */
  def sendFile(receiveId: String = cfg.chatId.getOrElse(""), fileKey: String): String =
    sendMsg(receiveId, "file", s"""{"file_key":${jsonStr(fileKey)}}""")

  private def sendMsg(receiveId: String, msgType: String, contentJson: String): String = {
    require(receiveId.nonEmpty, "receiveId is empty: pass chat_id/open_id or set config chatId")
    val json =
      s"""{"receive_id":${jsonStr(receiveId)},"msg_type":"$msgType","content":${jsonStr(contentJson)}}"""
    call("/open-apis/im/v1/messages?receive_id_type=chat_id", json)
  }

  // ---------------- 文件上传 ----------------
  /**
   * 上传文件到飞书（multipart/form-data），返回 file_key。
   * fileType：stream（普通文件）/ image / audio / video / file（均可），详见飞书文档。
   */
  def uploadFile(file: Path, fileName: String = null, fileType: String = "stream"): String = {
    val name = if (fileName == null) file.getFileName.toString else fileName
    val boundary = "----FeishuBoundary" + System.nanoTime()
    val bytes = Files.readAllBytes(file)

    val body = new java.io.ByteArrayOutputStream()
    def field(name: String, value: String): Unit = {
      body.write(s"--$boundary\r\nContent-Disposition: form-data; name=\"$name\"\r\n\r\n".getBytes(StandardCharsets.UTF_8))
      body.write(value.getBytes(StandardCharsets.UTF_8))
      body.write("\r\n".getBytes(StandardCharsets.UTF_8))
    }
    field("file_type", fileType)
    field("file_name", name)
    body.write(s"--$boundary\r\nContent-Disposition: form-data; name=\"file\"; filename=\"$name\"\r\nContent-Type: application/octet-stream\r\n\r\n".getBytes(StandardCharsets.UTF_8))
    body.write(bytes)
    body.write(s"\r\n--$boundary--\r\n".getBytes(StandardCharsets.UTF_8))

    val url = cfg.baseUrl + "/open-apis/im/v1/files"
    val req = HttpRequest.newBuilder()
      .uri(URI.create(url))
      .timeout(Duration.ofSeconds(60))
      .header("Authorization", s"Bearer ${tenantAccessToken()}")
      .header("Content-Type", s"multipart/form-data; boundary=$boundary")
      .POST(HttpRequest.BodyPublishers.ofByteArray(body.toByteArray))
      .build()
    val resp = http.send(req, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8))
    val bodyStr = resp.body()
    if (!respOk(bodyStr)) sys.error(s"uploadFile failed: $bodyStr")
    extractString(bodyStr, "file_key").getOrElse(sys.error(s"file_key missing in: $bodyStr"))
  }

  // ---------------- JSON 工具（最小实现，避免额外依赖） ----------------
  /** 校验响应 code==0 */
  def respOk(resp: String): Boolean = extractLong(resp, "code").contains(0L)

  /** 从响应 JSON 提取字段（简单正则，够用于扁平响应；嵌套响应可自行用 json4s） */
  def extractString(resp: String, key: String): Option[String] = {
    val re = (s""""$key"\\s*:\\s*"((?:[^"\\\\]|\\\\.)*)""").r
    re.findFirstMatchIn(resp).map(_.group(1))
  }
  def extractLong(resp: String, key: String): Option[Long] = {
    val re = (s""""$key"\\s*:\\s*(-?\\d+)""").r
    re.findFirstMatchIn(resp).map(_.group(1).toLong)
  }
  /** JSON 字符串转义 */
  def jsonStr(s: String): String =
    "\"" + s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case c if c < 0x20 => f"\\u${c.toInt}%04x"
      case c => c.toString
    } + "\""
}

object FeishuClient {
  /** 便捷工厂：从默认配置（系统属性/环境变量/feishu.conf）构建 */
  def apply(): FeishuClient = new FeishuClient(FeishuConfig.load())

  /** 从显式配置构建 */
  def apply(cfg: FeishuConfig): FeishuClient = new FeishuClient(cfg)

  /** 默认配置构建（别名） */
  def fromConfig(cfg: FeishuConfig): FeishuClient = new FeishuClient(cfg)
}
