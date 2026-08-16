package Feishu

/**
 * 飞书客户端演示 / 冒烟测试入口。
 *
 * 用法：
 *  - 仅验证配置加载与 token 获取：`sbt "runMain Feishu.FeishuDemo"`
 *  - 发送文本消息（需要 chat_id）：`sbt "runMain Feishu.FeishuDemo send 你好"`
 *  - 发送卡片消息：`sbt "runMain Feishu.FeishuDemo card"`
 *  - 上传文件：`sbt "runMain Feishu.FeishuDemo upload <文件路径>"`
 *
 * 凭据来源：feishu.conf（本地，gitignore）或环境变量 FEISHU_APP_ID/FEISHU_APP_SECRET/FEISHU_CHAT_ID。
 */
object FeishuDemo {
  def main(args: Array[String]): Unit = {
    val cfg = FeishuConfig.load()
    println(s"config loaded: appId=${cfg.appId} chatId=${cfg.chatId.getOrElse("(未设置)")}")

    val client = FeishuClient(cfg)

    args.headOption.getOrElse("token") match {
      case "send" =>
        val text = args.lift(1).getOrElse("来自 RegCbb 的飞书接入测试 🚀")
        require(cfg.chatId.nonEmpty, "send 需要 chatId：请在 feishu.conf 填写或设 FEISHU_CHAT_ID")
        val resp = client.sendText(cfg.chatId.get, text)
        println(if (client.respOk(resp)) "send ok" else s"send failed: $resp")

      case "card" =>
        require(cfg.chatId.nonEmpty, "card 需要 chatId")
        val resp = client.sendCard(cfg.chatId.get, "RegCbb 测试卡片", "**状态**：接入成功 ✅")
        println(if (client.respOk(resp)) "card ok" else s"card failed: $resp")

      case "upload" =>
        require(args.length >= 2, "upload 需要文件路径")
        require(cfg.chatId.nonEmpty, "upload 需要 chatId")
        val key = client.uploadFile(java.nio.file.Paths.get(args(1)))
        println(s"uploaded file_key=$key")
        val resp = client.sendFile(cfg.chatId.get, key)
        println(if (client.respOk(resp)) "sendFile ok" else s"sendFile failed: $resp")

      case _ => // token（默认）
        val token = client.tenantAccessToken()
        println(s"tenant_access_token=${token.take(12)}...(${token.length} chars)")
        println("token 获取成功，飞书凭据有效 ✓")
    }
  }
}
