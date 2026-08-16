package Feishu

import scala.util.Try
import java.nio.file.{Files, Path, Paths}

/**
 * 飞书应用配置。
 *
 * 配置来源（优先级从高到低）：
 *  1. JVM 系统属性：`feishu.appId` / `feishu.appSecret` / `feishu.chatId`
 *  2. 环境变量：`FEISHU_APP_ID` / `FEISHU_APP_SECRET` / `FEISHU_CHAT_ID`
 *  3. 本地配置文件：`./feishu.conf`（或被 `FEISHU_CONF` 指定的路径），Java Properties 格式：
 *     {{{
 *     appId=cli_xxxx
 *     appSecret=xxxx
 *     chatId=oc_xxxx
 *     }}}
 *
 * 凭据属敏感信息：`feishu.conf` 已被 .gitignore 忽略，请勿提交到版本库；
 * 模板见 `feishu.conf.example`。
 */
case class FeishuConfig(
  appId: String,
  appSecret: String,
  /** 默认推送目标（群 chat_id 或用户 open_id）；可为空，调用时再指定 */
  chatId: Option[String] = None,
  /** 飞书开放平台域名（默认中国大陆；海外版可改 https://open.larksuite.com） */
  baseUrl: String = "https://open.feishu.cn"
)

object FeishuConfig {

  /** 默认配置文件路径：./feishu.conf */
  val defaultConfFile: Path = Paths.get("feishu.conf")

  /** 从默认来源加载配置；缺少 appId/appSecret 时抛出明确错误 */
  def load(): FeishuConfig = load(defaultConfFile)

  /** 从指定配置文件加载（文件不存在时仅依赖系统属性/环境变量） */
  def load(confFile: Path): FeishuConfig = {
    val props = new java.util.Properties()
    if (Files.isReadable(confFile)) {
      val in = Files.newInputStream(confFile)
      try props.load(in) finally in.close()
    }

    def fromProps(k: String): Option[String] = Option(props.getProperty(k)).map(_.trim).filter(_.nonEmpty)
    def fromEnv(k: String): Option[String] = sys.env.get(k).map(_.trim).filter(_.nonEmpty)
    def fromSys(k: String): Option[String] = sys.props.get(k).map(_.trim).filter(_.nonEmpty)

    // 优先级：系统属性 > 环境变量 > 配置文件
    def resolve(sysKey: String, envKey: String, propKey: String): Option[String] =
      fromSys(sysKey).orElse(fromEnv(envKey)).orElse(fromProps(propKey))

    val appId = resolve("feishu.appId", "FEISHU_APP_ID", "appId")
      .getOrElse(sys.error(
        "FeishuConfig: missing appId. Set feishu.appId / FEISHU_APP_ID / feishu.conf(appId)."))
    val appSecret = resolve("feishu.appSecret", "FEISHU_APP_SECRET", "appSecret")
      .getOrElse(sys.error(
        "FeishuConfig: missing appSecret. Set feishu.appSecret / FEISHU_APP_SECRET / feishu.conf(appSecret)."))
    val chatId = resolve("feishu.chatId", "FEISHU_CHAT_ID", "chatId")
    val baseUrl = resolve("feishu.baseUrl", "FEISHU_BASE_URL", "baseUrl").getOrElse("https://open.feishu.cn")

    FeishuConfig(appId, appSecret, chatId, baseUrl)
  }

  /** 生成 feishu.conf 模板内容（占位符，供用户填写后重命名为 feishu.conf） */
  def exampleContent: String =
    """# Feishu 应用配置（复制为 feishu.conf 并填写；feishu.conf 已被 .gitignore 忽略）
      |appId=cli_xxxxxxxxxxxxxxxx
      |appSecret=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
      |# 默认推送目标（群 chat_id 或用户 open_id）；留空则调用时通过参数指定
      |chatId=
      |# 飞书开放平台域名（大陆版默认；海外版可改 https://open.larksuite.com）
      |baseUrl=https://open.feishu.cn
      |""".stripMargin
}
