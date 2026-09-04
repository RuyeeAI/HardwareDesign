package P4C

import P4C.Ast._
import P4C.Lexer._

final class P4Error(msg: String) extends RuntimeException(msg)

/** P4-16 子集递归下降语法分析。 */
class Parser(toks0: Seq[Tok]) {
  private val toks = toks0.toArray
  private var pos = 0

  private def cur: Tok = toks(pos)
  private def line: Int = cur.line
  private def err(msg: String): Nothing = throw new P4Error(s"语法错误（行 ${cur.line}）：$msg（附近: '${cur.text}'）")

  private def is(s: String): Boolean = cur.text == s && (cur.kind == TSym || cur.kind == TIdent)
  private def isIdent(s: String): Boolean = cur.kind == TIdent && cur.text == s
  private def eat(s: String): Unit = { if (!is(s)) err(s"期望 '$s'"); pos += 1 }
  private def eatIdent(): String = { if (cur.kind != TIdent) err("期望标识符"); val r = cur.text; pos += 1; r }
  private def eatNum(): BigInt = { if (cur.kind != TNum) err("期望数字"); val v = cur.value.get; pos += 1; v }
  private def accept(s: String): Boolean = { if (is(s)) { pos += 1; true } else false }

  private def atTypeStart: Boolean =
    (cur.kind == TIdent && (cur.text == "bit" || cur.text == "int" || cur.text == "varbit")) ||
      (cur.kind == TIdent && toks(pos + 1).kind == TIdent && toks(pos + 2).kind == TSym && toks(pos + 2).text == ";")

  private def parseType(): P4Type = {
    if (isIdent("bit") || isIdent("int") || isIdent("varbit")) {
      eat(cur.text)
      eat("<")
      val w = eatNum().toInt
      eat(">")
      BitsType(w)
    } else NamedType(eatIdent())
  }

  // ---------------- 程序 ----------------

  def parseProgram(): P4Program = {
    val headerTypes = scala.collection.mutable.ArrayBuffer.empty[HeaderType]
    val structs = scala.collection.mutable.ArrayBuffer.empty[StructType]
    val controls = scala.collection.mutable.ArrayBuffer.empty[ControlDecl]
    val parsers = scala.collection.mutable.ArrayBuffer.empty[ParserDecl]
    while (cur.kind != TEOF) {
      if (isIdent("header")) headerTypes += parseHeaderType()
      else if (isIdent("struct")) structs += parseStruct()
      else if (isIdent("control")) controls += parseControl()
      else if (isIdent("parser")) parsers += parseParser()
      else skipDecl() // package / typedef / const / instantiation 等：跳过
    }
    P4Program(headerTypes.toSeq, structs.toSeq, controls.toSeq, parsers.toSeq)
  }

  /** 跳过一个顶层声明：前进到与当前位置配对的 ';' 或 '{...}' 结束。 */
  private def skipDecl(): Unit = {
    // 跳过声明头（可能带注解 @...）直到 '{' 或 ';'
    var depth = 0
    var seenBrace = false
    while (cur.kind != TEOF) {
      if (cur.text == "{") { depth += 1; seenBrace = true; pos += 1 }
      else if (cur.text == "}") {
        depth -= 1; pos += 1
        if (seenBrace && depth == 0) return
      } else if (cur.text == ";" && depth == 0) { pos += 1; return }
      else pos += 1
    }
  }

  private def parseHeaderType(): HeaderType = {
    eat("header"); val name = eatIdent(); eat("{")
    val fields = scala.collection.mutable.ArrayBuffer.empty[HeaderField]
    while (!is("}")) {
      val t = parseType()
      val fname = eatIdent()
      eat(";")
      t match {
        case BitsType(w) => fields += HeaderField(fname, w)
        case _ => err("header 字段必须是 bit<N>")
      }
    }
    eat("}")
    HeaderType(name, fields.toSeq)
  }

  private def parseStruct(): StructType = {
    eat("struct"); val name = eatIdent(); eat("{")
    val members = scala.collection.mutable.ArrayBuffer.empty[StructMember]
    while (!is("}")) {
      val t = parseType()
      val mname = eatIdent()
      eat(";")
      t match {
        case BitsType(w) => members += StructMember("", isBits = true, w, mname)
        case NamedType(tn) => members += StructMember(tn, isBits = false, 0, mname)
      }
    }
    eat("}")
    StructType(name, members.toSeq)
  }

  // ---------------- parser ----------------

  /** parser 形参：`(packet_in pkt, out headers_t hdr, ...)`。方向可省略（如 packet_in）。 */
  private def parseParser(): ParserDecl = {
    val ln = line
    eat("parser")
    val name = eatIdent()
    eat("(")
    val params = scala.collection.mutable.ArrayBuffer.empty[ControlParam]
    def parseOneParam(): ControlParam = {
      val dir = if (isIdent("inout") || isIdent("out") || isIdent("in")) { val d = cur.text; pos += 1; d }
                else "in" // packet_in pkt：无方向关键字
      val tn = eatIdent()
      val pn = eatIdent()
      ControlParam(pn, dir, tn, line)
    }
    if (!is(")")) {
      params += parseOneParam()
      while (accept(",")) params += parseOneParam()
    }
    eat(")")
    eat("{")
    val states = scala.collection.mutable.ArrayBuffer.empty[ParserState]
    while (isIdent("state")) {
      eat("state")
      val sname = eatIdent()
      eat("{")
      val stmts = parseParserStmtsUntil("}")
      eat("}")
      states += ParserState(sname, stmts.toSeq, line)
    }
    eat("}")
    ParserDecl(name, params.toSeq, states.toSeq, ln)
  }

  /** parser 状态内语句：extract / transition / select / 普通赋值。 */
  private def parseParserStmtsUntil(end: String): Seq[Stmt] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Stmt]
    while (!is(end) && cur.kind != TEOF) {
      val ln = line
      if (isIdent("transition")) {
        eat("transition")
        if (isIdent("select")) {
          eat("select")
          eat("(")
          val value = parseExpr()
          eat(")")
          eat("{")
          val cases = scala.collection.mutable.ArrayBuffer.empty[(Expr, String)]
          var deft = "accept"
          while (!is("}") && cur.kind != TEOF) {
            if (isIdent("default")) {
              eat("default"); eat(":"); deft = eatIdent(); eat(";")
            } else {
              val pat = parseExpr()
              eat(":")
              val tgt = eatIdent()
              eat(";")
              cases += ((pat, tgt))
            }
          }
          eat("}")
          out += Select(value, cases.toSeq, deft, ln)
        } else {
          val target = eatIdent()
          eat(";")
          out += Goto(target, ln)
        }
      } else {
        // pkt.extract(hdr.x); / hdr.setValid(); / 普通赋值
        val first = eatIdent()
        if (accept(".")) {
          val seg = eatIdent()
          if (accept("(")) {
            if (seg == "extract") {
              // 参数：hdr.instance
              val p0 = eatIdent()
              val ppath = scala.collection.mutable.ArrayBuffer(p0)
              while (accept(".")) ppath += eatIdent()
              eat(")")
              eat(";")
              out += Extract(ppath.toSeq, ln)
            } else { // setValid 等无参方法：跳过
              if (!is(")")) err(s"parser 中不支持带参方法 '$seg'")
              eat(")")
              eat(";")
            }
          } else {
            // 字段访问开头，回落到普通赋值解析
            val path = scala.collection.mutable.ArrayBuffer(first, seg)
            while (accept(".")) path += eatIdent()
            eat("=")
            val e = parseExpr()
            eat(";")
            out += Assign(path.toSeq, e, ln)
          }
        } else {
          err(s"parser 状态中不支持的语句开头 '$first'")
        }
      }
    }
    out.toSeq
  }

  // ---------------- control ----------------

  private def parseControl(): ControlDecl = {
    eat("control")
    val name = eatIdent()
    // 可选类型参数 <...>：跳过
    if (accept("<")) { var d = 1; while (d > 0 && cur.kind != TEOF) { if (cur.text == "<") d += 1 else if (cur.text == ">") d -= 1; pos += 1 } }
    eat("(")
    val params = scala.collection.mutable.ArrayBuffer.empty[ControlParam]
    if (!is(")")) {
      params += {
        val dir = eatIdent() // inout / in / out
        val tn = eatIdent()
        val pn = eatIdent()
        ControlParam(pn, dir, tn, line)
      }
      while (accept(",")) {
        val dir = eatIdent()
        val tn = eatIdent()
        val pn = eatIdent()
        params += ControlParam(pn, dir, tn, line)
      }
    }
    eat(")")
    eat("{")
    val actions = scala.collection.mutable.ArrayBuffer.empty[ActionDecl]
    val tables = scala.collection.mutable.ArrayBuffer.empty[TableDecl]
    val externs = scala.collection.mutable.ArrayBuffer.empty[ExternInst]
    var applyBody: Seq[Stmt] = Seq.empty
    while (!is("}")) {
      if (isIdent("action")) actions += parseAction()
      else if (isIdent("table")) tables += parseTable()
      else if (isIdent("Register") || isIdent("Counter")) externs += parseExternInst()
      else if (isIdent("apply")) { eat("apply"); eat("{"); applyBody = parseStmtsUntil("}") }
      else skipStmt() // 未知语句（注解、default_action 残留等）
    }
    eat("}")
    ControlDecl(name, params.toSeq, actions.toSeq, tables.toSeq, externs.toSeq, applyBody, line)
  }

  /** `Register(bit<16>, 8) name;` / `Counter(bit<32>, 8) name;` */
  private def parseExternInst(): ExternInst = {
    val ln = line
    val kind = eatIdent()
    if (kind != "Register" && kind != "Counter") err(s"未知 extern 类型 '$kind'")
    eat("(")
    val w = parseType() match {
      case BitsType(w) => w
      case _ => err("extern 元素类型必须是 bit<N>")
    }
    eat(",")
    val size = eatNum().toInt
    eat(")")
    val name = eatIdent()
    eat(";")
    ExternInst(kind, w, size, name, ln)
  }

  private def skipStmt(): Unit = {
    var depth = 0
    while (cur.kind != TEOF) {
      val t = cur.text
      pos += 1
      if (t == "{") depth += 1
      else if (t == "}") { depth -= 1; if (depth <= 0) return }
      else if (t == ";" && depth == 0) return
    }
  }

  private def parseAction(): ActionDecl = {
    val ln = line
    eat("action")
    val name = eatIdent()
    eat("(")
    val params = scala.collection.mutable.ArrayBuffer.empty[Param]
    if (!is(")")) {
      params += {
        val t = parseType() match {
          case BitsType(w) => w
          case _ => err("action 参数必须是 bit<N>")
        }
        val pn = eatIdent()
        Param(pn, t, line)
      }
      while (accept(",")) {
        val t = parseType() match {
          case BitsType(w) => w
          case _ => err("action 参数必须是 bit<N>")
        }
        val pn = eatIdent()
        params += Param(pn, t, line)
      }
    }
    eat(")")
    eat("{")
    val body = parseStmtsUntil("}")
    eat("}")
    ActionDecl(name, params.toSeq, body, ln)
  }

  private def parseTable(): TableDecl = {
    val ln = line
    eat("table")
    val name = eatIdent()
    eat("{")
    val keys = scala.collection.mutable.ArrayBuffer.empty[KeyElem]
    val actions = scala.collection.mutable.ArrayBuffer.empty[String]
    val entries = scala.collection.mutable.ArrayBuffer.empty[TableEntry]
    while (!is("}")) {
      if (isIdent("key")) {
        eat("key"); eat("="); eat("{")
        while (!is("}")) {
          val e = parseExpr()
          eat(":")
          val kind = eatIdent()
          eat(";")
          keys += KeyElem(e, kind, ln)
        }
        eat("}")
      } else if (isIdent("actions")) {
        eat("actions"); eat("="); eat("{")
        while (!is("}")) { actions += eatIdent(); eat(";") }
        eat("}")
      } else if (isIdent("const") && toks(pos + 1).text == "entries") {
        eat("const"); eat("entries"); eat("="); eat("{")
        while (!is("}")) entries += parseTableEntry()
        eat("}")
      } else skipStmt()
    }
    eat("}")
    TableDecl(name, keys.toSeq, actions.toSeq, entries.toSeq, ln)
  }

  private def parseTableEntry(): TableEntry = {
    val ln = line
    val isDefault = isIdent("default")
    val keys = scala.collection.mutable.ArrayBuffer.empty[Expr]
    if (isDefault) eat("default")
    else {
      keys += parseExpr()
      while (accept(",")) keys += parseExpr()
    }
    eat(":")
    val act = eatIdent()
    val args = scala.collection.mutable.ArrayBuffer.empty[Expr]
    if (accept("(")) {
      if (!is(")")) {
        args += parseExpr()
        while (accept(",")) args += parseExpr()
      }
      eat(")")
    }
    eat(";")
    TableEntry(keys.toSeq, isDefault, act, args.toSeq, ln)
  }

  // ---------------- 语句 ----------------

  private def parseStmtsUntil(end: String): Seq[Stmt] = {
    val out = scala.collection.mutable.ArrayBuffer.empty[Stmt]
    while (!is(end) && cur.kind != TEOF) out += parseStmt()
    out.toSeq
  }

  private def parseStmt(): Stmt = {
    val ln = line
    // 变量声明：bit<N> name = expr?; 或 TypeName name = expr?;
    if (atTypeStart && !(cur.kind == TIdent && toks(pos+1).kind == TSym && toks(pos+1).text == ".")) {
      parseType() match {
        case BitsType(w) =>
          val n = eatIdent()
          val init = if (accept("=")) Some(parseExpr()) else None
          eat(";")
          return VarDecl(n, w, init, ln)
        case _ => err("局部变量必须是 bit<N>")
      }
    }
    // 路径开头
    val first = eatIdent()
    if (accept("(")) { // action 调用
      val args = scala.collection.mutable.ArrayBuffer.empty[Expr]
      if (!is(")")) {
        args += parseExpr()
        while (accept(",")) args += parseExpr()
      }
      eat(")")
      eat(";")
      return ActionCall(first, args.toSeq, ln)
    }
    val path = scala.collection.mutable.ArrayBuffer(first)
    while (accept(".")) {
      val seg = eatIdent()
      if (accept("(")) { // tbl.apply() / inst.write(...)/inst.count(...)
        val args = scala.collection.mutable.ArrayBuffer.empty[Expr]
        if (!is(")")) {
          args += parseExpr()
          while (accept(",")) args += parseExpr()
        }
        eat(")")
        eat(";")
        if (seg == "apply") return TableApply(first, ln)
        return MethodCall(first, seg, args.toSeq, ln)
      }
      path += seg
    }
    // 切片左值？hdr.f[3:0] = x —— M1 不支持，报错
    if (is("[")) err("暂不支持对切片赋值")
    eat("=")
    val e = parseExpr()
    eat(";")
    Assign(path.toSeq, e, ln)
  }

  // ---------------- 表达式（优先级：三元 < 拼接 < || < && < | < ^ < & < 相等 < 关系 < 移位 < 加减 < 乘除 < 一元） ----------------

  def parseExpr(): Expr = parseTernary()

  private def parseTernary(): Expr = {
    val c = parseConcat()
    if (accept("?")) {
      val ln = line
      val t = parseExpr()
      eat(":")
      val f = parseTernary()
      Ternary(c, t, f, ln)
    } else c
  }

  private def parseConcat(): Expr = {
    var l = parseOr()
    while (is("++")) { val ln = line; pos += 1; val r = parseOr(); l = Bin("++", l, r, ln) }
    l
  }

  private def parseOr(): Expr = {
    var l = parseAnd()
    while (is("||")) { val ln = line; pos += 1; val r = parseAnd(); l = Bin("||", l, r, ln) }
    l
  }

  private def parseAnd(): Expr = {
    var l = parseBitor()
    while (is("&&")) { val ln = line; pos += 1; val r = parseBitor(); l = Bin("&&", l, r, ln) }
    l
  }

  private def parseBitor(): Expr = {
    var l = parseBitxor()
    while (is("|") && !is("||")) { val ln = line; pos += 1; val r = parseBitxor(); l = Bin("|", l, r, ln) }
    l
  }

  private def parseBitxor(): Expr = {
    var l = parseBitand()
    while (is("^")) { val ln = line; pos += 1; val r = parseBitand(); l = Bin("^", l, r, ln) }
    l
  }

  private def parseBitand(): Expr = {
    var l = parseEq()
    while (is("&") && !is("&&")) { val ln = line; pos += 1; val r = parseEq(); l = Bin("&", l, r, ln) }
    l
  }

  private def parseEq(): Expr = {
    var l = parseRel()
    while (is("==") || is("!=")) { val ln = line; val op = cur.text; pos += 1; val r = parseRel(); l = Bin(op, l, r, ln) }
    l
  }

  private def parseRel(): Expr = {
    var l = parseShift()
    while (is("<") && !is("<<") || is(">") && !is(">>") || is("<=") || is(">=")) {
      val ln = line; val op = cur.text; pos += 1; val r = parseShift(); l = Bin(op, l, r, ln)
    }
    l
  }

  private def parseShift(): Expr = {
    var l = parseAdd()
    while (is("<<") || is(">>")) { val ln = line; val op = cur.text; pos += 1; val r = parseAdd(); l = Bin(op, l, r, ln) }
    l
  }

  private def parseAdd(): Expr = {
    var l = parseMul()
    while (is("+") || is("-")) { val ln = line; val op = cur.text; pos += 1; val r = parseMul(); l = Bin(op, l, r, ln) }
    l
  }

  private def parseMul(): Expr = {
    var l = parseUnary()
    while (is("*") || is("/") || is("%")) { val ln = line; val op = cur.text; pos += 1; val r = parseUnary(); l = Bin(op, l, r, ln) }
    l
  }

  private def parseUnary(): Expr = {
    if (is("~")) { val ln = line; pos += 1; Un("~", parseUnary(), ln) }
    else if (is("!")) { val ln = line; pos += 1; Un("!", parseUnary(), ln) }
    else parsePostfix()
  }

  private def parsePostfix(): Expr = {
    var e = parsePrimary()
    while (is("[")) {
      val ln = line
      pos += 1
      val hi = eatNum().toInt
      eat(":")
      val lo = eatNum().toInt
      eat("]")
      e = Slice(e, hi, lo, ln)
    }
    e
  }

  private def parsePrimary(): Expr = {
    val ln = line
    cur match {
      case Tok(TNum, _, Some(v), w, _) => pos += 1; Num(v, w, ln)
      case Tok(TSym, "(", _, _, _) =>
        pos += 1
        if (isIdent("bit") || isIdent("int")) { // 类型转换 (bit<N>)expr
          eat(cur.text); eat("<")
          val w = eatNum().toInt
          eat(">"); eat(")")
          Cast(w, parseUnary(), ln)
        } else {
          val e = parseExpr(); eat(")"); e
        }
      case Tok(TIdent, _, _, _, _) =>
        val first = eatIdent()
        val path = scala.collection.mutable.ArrayBuffer(first)
        while (accept(".")) path += eatIdent()
        if (is("(")) { // extern 读方法：inst.read(idx)
          pos += 1
          val args = scala.collection.mutable.ArrayBuffer.empty[Expr]
          if (!is(")")) {
            args += parseExpr()
            while (accept(",")) args += parseExpr()
          }
          eat(")")
          Call(path.toSeq, args.toSeq, ln)
        } else Name(path.toSeq, ln)
      case _ => err("期望表达式")
    }
  }
}

object Parser {
  def parseProgram(src: String): P4Program = {
    val toks = Lexer.tokenize(Preprocess(src))
    new Parser(toks).parseProgram()
  }
}
