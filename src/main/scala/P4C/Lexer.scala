package P4C

/** P4-16 子集词法分析。
  *
  * 输入文本已由 [[Preprocess]] 去掉注释和预处理行。支持：
  *   - 带宽字面量：`16w255`、`16w0x1f`
  *   - 普通字面量：`0x0800`、`255`
  *   - 标识符、符号（最长匹配：<< >> == != <= >= ++ && ||）
  */
object Lexer {

  sealed trait TokKind
  case object TIdent extends TokKind
  case object TNum extends TokKind
  case object TSym extends TokKind
  case object TEOF extends TokKind

  final case class Tok(kind: TokKind, text: String, value: Option[BigInt], width: Option[Int], line: Int)

  private val symbolList = Seq(
    "<<", ">>", "==", "!=", "<=", ">=", "++", "&&", "||",
    "{", "}", "(", ")", "[", "]", ";", ",", ":", "=", "<", ">",
    "+", "-", "*", "/", "%", "|", "&", "^", "~", "?", ".", "!", "@",
  )

  private val identRe = "[A-Za-z_][A-Za-z0-9_]*".r
  private val widthNumRe = "([0-9]+)w(0[xX][0-9a-fA-F]+|[0-9]+)".r
  private val hexNumRe = "0[xX][0-9a-fA-F]+".r
  private val decNumRe = "[0-9]+".r

  private def parseNum(s: String): BigInt =
    if (s.toLowerCase.startsWith("0x")) BigInt(s.substring(2), 16) else BigInt(s)

  def tokenize(src: String): Seq[Tok] = {
    val toks = scala.collection.mutable.ArrayBuffer.empty[Tok]
    var line = 1
    var i = 0
    val n = src.length
    def err(msg: String) = throw new P4Error(s"词法错误（行 $line）：$msg")
    while (i < n) {
      val c = src.charAt(i)
      if (c == '\n') { line += 1; i += 1 }
      else if (c.isWhitespace) i += 1
      else if (c == '/' && i + 1 < n && src.charAt(i + 1) == '/') { while (i < n && src.charAt(i) != '\n') i += 1 }
      else if (c == '/' && i + 1 < n && src.charAt(i + 1) == '*') {
        while (i + 1 < n && !(src.charAt(i) == '*' && src.charAt(i + 1) == '/')) { if (src.charAt(i) == '\n') line += 1; i += 1 }
        if (i + 1 >= n) err("块注释未闭合")
        i += 2
      }
      else {
        val rest = src.substring(i)
        var matched = false
        // 带宽字面量
        widthNumRe.findPrefixMatchOf(rest).foreach { m =>
          toks += Tok(TNum, m.matched, Some(parseNum(m.group(2))), Some(m.group(1).toInt), line)
          i += m.matched.length; matched = true
        }
        if (!matched) hexNumRe.findPrefixOf(rest).foreach { m =>
          toks += Tok(TNum, m, Some(parseNum(m)), None, line); i += m.length; matched = true
        }
        if (!matched) decNumRe.findPrefixOf(rest).foreach { m =>
          toks += Tok(TNum, m, Some(parseNum(m)), None, line); i += m.length; matched = true
        }
        if (!matched) identRe.findPrefixOf(rest).foreach { m =>
          toks += Tok(TIdent, m, None, None, line); i += m.length; matched = true
        }
        if (!matched) {
          symbolList.find(s => rest.startsWith(s)).foreach { s =>
            toks += Tok(TSym, s, None, None, line); i += s.length; matched = true
          }
        }
        if (!matched) err(s"无法识别的字符 '$c'")
      }
    }
    toks += Tok(TEOF, "<eof>", None, None, line)
    toks.toSeq
  }
}

/** 去掉块注释、行注释和预处理指令行（#include、#define 等）。 */
object Preprocess {
  def apply(src: String): String = {
    val sb = new StringBuilder
    var i = 0
    val n = src.length
    var lineStart = true
    while (i < n) {
      val c = src.charAt(i)
      if (lineStart && c == '#') { // 跳过预处理行
        while (i < n && src.charAt(i) != '\n') i += 1
        lineStart = false
      } else if (c == '/' && i + 1 < n && src.charAt(i + 1) == '/') {
        while (i < n && src.charAt(i) != '\n') i += 1
      } else if (c == '/' && i + 1 < n && src.charAt(i + 1) == '*') {
        i += 2
        while (i + 1 < n && !(src.charAt(i) == '*' && src.charAt(i + 1) == '/')) i += 1
        i += 2
      } else {
        sb.append(c)
        lineStart = c == '\n'
        i += 1
      }
    }
    sb.toString
  }
}
