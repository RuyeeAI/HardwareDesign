package BaseCbb.RegCbb.gen

import BaseCbb.RegCbb._

/**
 * HTML 寄存器手册生成器（自包含，无外部依赖）。
 * 输出：左侧侧边栏（寄存器/存储器导航，可跳转）+ 地址映射表 + 每寄存器位域图（div 按位宽比例）
 *      + 字段表（含枚举）。
 */
object HtmlGen {

  def generate(map: RegFileMap): String = {
    val sb = new StringBuilder
    sb ++= "<!DOCTYPE html>\n<html lang=\"zh-CN\">\n<head>\n<meta charset=\"utf-8\">\n"
    sb ++= s"<title>${map.deviceName} 寄存器手册</title>\n"
    sb ++= "<style>\n"
    sb ++= "html,body{margin:0;padding:0;height:100%;}\n"
    sb ++= "body{font-family:'Helvetica Neue',Arial,'PingFang SC','Microsoft YaHei',sans-serif;color:#222;display:flex;}\n"
    sb ++= "/* ===== 左侧侧边栏 ===== */\n"
    sb ++= "#sidebar{width:250px;min-width:250px;height:100vh;position:sticky;top:0;overflow-y:auto;\n"
    sb ++= "  background:#f5f6f8;border-right:1px solid #d8dbe0;padding:14px 10px;box-sizing:border-box;font-size:13px;}\n"
    sb ++= "#sidebar h1{font-size:15px;margin:0 0 10px 6px;border:none;padding:0;}\n"
    sb ++= "#sidebar a{color:#234;text-decoration:none;display:block;padding:2px 6px;border-radius:4px;}\n"
    sb ++= "#sidebar a:hover{background:#e4e8ee;}\n"
    sb ++= "#sidebar .grp{font-weight:bold;color:#456;margin:8px 0 2px 6px;}\n"
    sb ++= "#sidebar ul{list-style:none;margin:0 0 6px 14px;padding:0;}\n"
    sb ++= "#sidebar li a{color:#567;}\n"
    sb ++= "/* ===== 主内容区 ===== */\n"
    sb ++= "#main{flex:1;min-width:0;padding:24px 32px;box-sizing:border-box;}\n"
    sb ++= "h1{border-bottom:3px solid #345;padding-bottom:8px;}\n"
    sb ++= "h2{margin-top:36px;border-bottom:1px solid #ccc;padding-bottom:4px;}\n"
    sb ++= "h2:target{background:#fff7d6;border-radius:4px;padding-left:6px;}\n"
    sb ++= "table{border-collapse:collapse;margin:12px 0;font-size:14px;}\n"
    sb ++= "th,td{border:1px solid #bbb;padding:5px 10px;text-align:left;}\n"
    sb ++= "th{background:#eef1f5;}\n"
    sb ++= "code{background:#f4f4f4;padding:1px 4px;border-radius:3px;}\n"
    sb ++= ".bitfield{display:flex;margin:10px 0;border:1px solid #888;min-height:34px;}\n"
    sb ++= ".bf{display:flex;flex-direction:column;justify-content:center;text-align:center;font-size:12px;\n"
    sb ++= "     border-right:1px solid #888;padding:2px;overflow:hidden;white-space:nowrap;}\n"
    sb ++= ".bf .nm{font-weight:bold;}\n"
    sb ++= ".bf .rn{color:#888;font-size:10px;}\n"
    sb ++= ".meta{color:#666;font-size:13px;}\n"
    sb ++= ".reg-overview{font-size:13px;}\n"
    sb ++= "</style>\n</head>\n<body>\n"

    // ==================== 侧边栏 ====================
    sb ++= "<nav id=\"sidebar\">\n"
    sb ++= s"<h1>${esc(map.deviceName)}</h1>\n"
    sb ++= "<a href=\"#top\">☰ 地址映射</a>\n"
    if (map.regs.nonEmpty) {
      sb ++= "<div class=\"grp\">寄存器</div>\n<ul>\n"
      map.regs.foreach { a =>
        sb ++= s"<li><a href=\"#reg_${anchor(a.reg.name)}\">${esc(a.reg.name)}</a></li>\n"
      }
      sb ++= "</ul>\n"
    }
    if (map.mems.nonEmpty) {
      sb ++= "<div class=\"grp\">存储器</div>\n<ul>\n"
      map.mems.foreach { ma =>
        sb ++= s"<li><a href=\"#mem_${anchor(ma.mem.name)}\">${esc(ma.mem.name)}</a></li>\n"
      }
      sb ++= "</ul>\n"
    }
    sb ++= "</nav>\n"

    // ==================== 主内容 ====================
    sb ++= "<main id=\"main\">\n"
    sb ++= s"<h1 id=\"top\">${esc(map.deviceName)} 寄存器手册</h1>\n"
    sb ++= s"<p class=\"meta\">由 RegCbb 自动生成 · 寄存器基地址 <code>${hex(map.regBaseAddress)}</code> · 存储器基地址 <code>${hex(map.memBaseAddress)}</code></p>\n"
    if (map.description.nonEmpty) sb ++= s"<p>${esc(map.description)}</p>\n"

    // 地址映射（寄存器名称可跳转）
    sb ++= "<h2>地址映射</h2>\n<table><tr><th>偏移</th><th>名称</th><th>大小</th><th>访问/原子</th><th>复位值</th><th>说明</th></tr>\n"
    map.regs.foreach { a =>
      val atom = if (a.wordCount > 1) (if (a.reg.atomic) "多字·原子" else "多字·非原子") else "-"
      sb ++= s"<tr><td><code>${hex(a.byteOffset)}</code></td>" +
        s"<td><a href=\"#reg_${anchor(a.reg.name)}\"><code>${esc(a.reg.name)}</code></a></td>" +
        s"<td>${a.byteSize} B</td><td>$atom</td><td><code>${hex(resetOf(a))}</code></td><td>${esc(a.reg.description)}</td></tr>\n"
    }
    map.mems.foreach { ma =>
      val atom = if (ma.mem.wordCount > 1) (if (ma.mem.atomic) "原子" else "非原子") else "-"
      sb ++= s"<tr><td><code>${hex(ma.baseAddress)}</code></td>" +
        s"<td><a href=\"#mem_${anchor(ma.mem.name)}\"><code>${esc(ma.mem.name)}</code></a>（存储器）</td>" +
        s"<td>${ma.mem.byteSize} B</td><td>$atom</td><td>-</td><td>${esc(ma.mem.description)}</td></tr>\n"
    }
    sb ++= "</table>\n"

    // 每寄存器
    map.regs.foreach { a =>
      val atom = if (a.wordCount > 1) (if (a.reg.atomic) "（多字原子：写低字暂存，写最高字提交）" else "（多字非原子：逐字直接写）") else ""
      sb ++= s"<h2 id=\"reg_${anchor(a.reg.name)}\">${esc(a.reg.name)} ${a.reg.group.map(g => s"<small>$g</small>").getOrElse("")}</h2>\n"
      sb ++= s"<p class=\"meta\">偏移 <code>${hex(a.byteOffset)}</code> · ${a.byteSize} B · 复位 <code>${hex(resetOf(a))}</code> $atom</p>\n"
      if (a.reg.description.nonEmpty) sb ++= s"<p>${esc(a.reg.description)}</p>\n"
      sb ++= bitfield(a)
      sb ++= "<table><tr><th>位</th><th>字段</th><th>访问</th><th>复位</th><th>描述</th></tr>\n"
      a.fieldAllocations.sortBy(-_.bitOffset).foreach { fa =>
        val f = fa.field
        val bits = if (f.bitWidth == 1) s"[${fa.bitOffset}]" else s"[${fa.bitOffset + f.bitWidth - 1}:${fa.bitOffset}]"
        val enums = f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
          s"<br/><code>0x${v.toString(16)}</code> = $n${if (d.nonEmpty) s"（$d）" else ""}"
        }.mkString
        sb ++= s"<tr><td>$bits</td><td><code>${esc(f.name)}</code></td><td>${f.access.id}</td>" +
          s"<td><code>${hex(f.resetValue)}</code></td><td>${esc(f.description)}$enums</td></tr>\n"
      }
      sb ++= "</table>\n"
    }

    if (map.mems.nonEmpty) {
      sb ++= "<h2>存储器</h2>\n<table><tr><th>名称</th><th>基地址</th><th>深度</th><th>位宽</th><th>大小</th><th>类型</th><th>原子</th><th>说明</th></tr>\n"
      map.mems.foreach { ma =>
        sb ++= s"<tr><td><code>${esc(ma.mem.name)}</code></td><td><code>${hex(ma.baseAddress)}</code></td>" +
          s"<td>${ma.mem.depth}</td><td>${ma.mem.dataWidth}</td><td>${ma.mem.byteSize} B</td>" +
          s"<td>${ma.mem.memType.id}</td><td>${ma.mem.atomic}</td><td>${esc(ma.mem.description)}</td></tr>\n"
        if (ma.mem.entryFields.nonEmpty) {
          sb ++= s"<tr><td colspan=\"8\"><b>${esc(ma.mem.name)} entry 域段</b>（位宽 ${ma.mem.dataWidth}，LSB-first）：" +
            "<table><tr><th>位</th><th>字段</th><th>访问</th><th>复位</th><th>描述</th></tr>\n"
          ma.mem.entryFields.zip(ma.mem.entryFieldOffsets).sortBy(-_._2).foreach { case (f, bitOffset) =>
            val bits = if (f.bitWidth == 1) s"[${bitOffset}]" else s"[${bitOffset + f.bitWidth - 1}:${bitOffset}]"
            val enums = f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
              s"<br/><code>0x${v.toString(16)}</code> = $n${if (d.nonEmpty) s"（$d）" else ""}"
            }.mkString
            sb ++= s"<tr><td>$bits</td><td><code>${esc(f.name)}</code></td><td>${f.access.id}</td>" +
              s"<td><code>${hex(f.resetValue)}</code></td><td>${esc(f.description)}$enums</td></tr>\n"
          }
          sb ++= "</table></td></tr>\n"
        }
      }
      sb ++= "</table>\n"
      // 存储器详细锚点（供侧边栏跳转）
      map.mems.foreach { ma =>
        sb ++= s"<h3 id=\"mem_${anchor(ma.mem.name)}\">${esc(ma.mem.name)}（${hex(ma.baseAddress)}，${ma.mem.depth}×${ma.mem.dataWidth}）</h3>\n"
      }
    }

    sb ++= "<hr/><p class=\"meta\">本文档由 RegCbb 自动生成，请勿手工修改。</p>\n"
    sb ++= "</main>\n"
    sb ++= "</body>\n</html>\n"
    sb.toString
  }

  /** 位域图：div 横向排列，宽度按位宽比例 */
  private def bitfield(a: RegAllocation): String = {
    val total = a.totalBits
    val fas = a.fieldAllocations.sortBy(-_.bitOffset) // MSB first
    val sb = new StringBuilder
    sb ++= "<div class=\"bitfield\">"
    fas.foreach { fa =>
      val pct = fa.field.bitWidth.toDouble / total * 100.0
      val bits = if (fa.field.bitWidth == 1) s"[${fa.bitOffset}]" else s"[${fa.bitOffset + fa.field.bitWidth - 1}:${fa.bitOffset}]"
      sb ++= s"""<div class="bf" style="width:${pct}%;" title="${esc(fa.field.description)}">""" +
        s"""<span class="nm">${esc(fa.field.name)}</span><span class="rn">$bits ${fa.field.access.id}</span></div>"""
    }
    sb ++= "</div>"
    sb.toString
  }

  /** 锚点 id 清洗：非字母数字下划线 → _ */
  private def anchor(s: String): String =
    if (s.isEmpty) "_" else s.map(c => if (c.isLetterOrDigit || c == '_') c else '_')

  private def resetOf(a: RegAllocation): BigInt =
    a.fieldAllocations.foldLeft(BigInt(0)) { case (acc, fa) => acc | (fa.field.resetValue << fa.bitOffset) }

  private def esc(s: String): String = s.flatMap {
    case '&' => "&amp;"
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case c   => c.toString
  }
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}
