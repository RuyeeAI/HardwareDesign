package BaseCbb.RegCbb.gen

import BaseCbb.RegCbb._

/**
 * 系统级文档/软件视图生成器（多功能模块）。
 * 输入 SystemMap（AddressAllocator.allocateSystem 产物），不依赖外围逻辑，纯 IR 生成。
 */

/** 系统级 JSON：设备 + 模块列表（含各模块地址、寄存器块、存储器块） */
object SystemJsonGen {
  def generate(sysMap: SystemMap): String = {
    val s = sysMap.system
    val sb = new StringBuilder
    sb ++= "{\n"
    sb ++= s"""  "systemName": ${q(s.name)},\n"""
    sb ++= s"""  "deviceName": ${q(s.devName)},\n"""
    sb ++= s"""  "description": ${q(s.description)},\n"""
    sb ++= "  \"modules\": [\n"
    sysMap.modules.zipWithIndex.foreach { case (ma, i) =>
      sb ++= "    {\n"
      sb ++= s"""      "name": ${q(ma.module.name)},\n"""
      sb ++= s"""      "baseAddress": ${q(hex(ma.baseAddress))},\n"""
      sb ++= s"""      "memBaseAddress": ${q(hex(ma.memBaseAddress))},\n"""
      sb ++= s"""      "sizeBytes": ${ma.sizeBytes},\n"""
      sb ++= s"""      "description": ${q(ma.module.description)},\n"""
      sb ++= "      \"regBlocks\": [\n"
      ma.regBlocks.zipWithIndex.foreach { case (rb, j) =>
        sb ++= "        {\n"
        sb ++= s"""          "name": ${q(rb.block.name)},\n"""
        sb ++= s"""          "baseAddress": ${q(hex(rb.baseAddress))},\n"""
        sb ++= "          \"registers\": [\n"
        rb.regs.zipWithIndex.foreach { case (a, k) =>
          sb ++= "            {\n"
          sb ++= s"""              "name": ${q(a.reg.name)},\n"""
          sb ++= s"""              "byteOffset": ${q(hex(a.byteOffset))},\n"""
          sb ++= s"""              "byteSize": ${a.byteSize},\n"""
          sb ++= s"""              "wordCount": ${a.wordCount},\n"""
          sb ++= s"""              "atomic": ${a.reg.atomic},\n"""
          sb ++= s"""              "description": ${q(a.reg.description)},\n"""
          sb ++= "              \"fields\": [\n"
          a.fieldAllocations.sortBy(-_.bitOffset).zipWithIndex.foreach { case (fa, l) =>
            val f = fa.field
            sb ++= "                {\n"
            sb ++= s"""                  "name": ${q(f.name)},\n"""
            sb ++= s"""                  "bitOffset": ${fa.bitOffset},\n"""
            sb ++= s"""                  "bitWidth": ${f.bitWidth},\n"""
            sb ++= s"""                  "access": ${q(f.access.id)},\n"""
            sb ++= s"""                  "writeAction": ${q(f.writeAction.id)},\n"""
            sb ++= s"""                  "resetValue": ${f.resetValue},\n"""
            sb ++= s"""                  "description": ${q(f.description)}\n"""
            sb ++= "                }" + (if (l < a.fieldAllocations.size - 1) "," else "") + "\n"
          }
          sb ++= "              ]\n"
          sb ++= "            }" + (if (k < rb.regs.size - 1) "," else "") + "\n"
        }
        sb ++= "          ]\n"
        sb ++= "        }" + (if (j < ma.regBlocks.size - 1) "," else "") + "\n"
      }
      sb ++= "      ],\n"
      sb ++= "      \"memBlocks\": [\n"
      ma.memBlocks.zipWithIndex.foreach { case (mb, j) =>
        sb ++= "        {\n"
        sb ++= s"""          "name": ${q(mb.block.name)},\n"""
        sb ++= s"""          "baseAddress": ${q(hex(mb.baseAddress))},\n"""
        sb ++= "          \"memories\": [\n"
        mb.mems.zipWithIndex.foreach { case (m, k) =>
          sb ++= "            {\n"
          sb ++= s"""              "name": ${q(m.mem.name)},\n"""
          sb ++= s"""              "baseAddress": ${q(hex(m.baseAddress))},\n"""
          sb ++= s"""              "depth": ${m.mem.depth},\n"""
          sb ++= s"""              "dataWidth": ${m.mem.dataWidth},\n"""
          sb ++= s"""              "byteSize": ${m.mem.byteSize},\n"""
          sb ++= s"""              "memType": ${q(m.mem.memType.id)},\n"""
          sb ++= s"""              "atomic": ${m.mem.atomic},\n"""
          sb ++= s"""              "description": ${q(m.mem.description)},\n"""
          sb ++= "              \"entryFields\": [\n"
          m.mem.entryFields.zip(m.mem.entryFieldOffsets).sortBy(-_._2).zipWithIndex.foreach { case ((f, bitOffset), l) =>
            sb ++= "                {\n"
            sb ++= s"""                  "name": ${q(f.name)},\n"""
            sb ++= s"""                  "bitOffset": $bitOffset,\n"""
            sb ++= s"""                  "bitWidth": ${f.bitWidth},\n"""
            sb ++= s"""                  "access": ${q(f.access.id)},\n"""
            sb ++= s"""                  "writeAction": ${q(f.writeAction.id)},\n"""
            sb ++= s"""                  "resetValue": ${f.resetValue},\n"""
            sb ++= s"""                  "description": ${q(f.description)}\n"""
            sb ++= "                }" + (if (l < m.mem.entryFields.size - 1) "," else "") + "\n"
          }
          sb ++= "              ]\n"
          sb ++= "            }" + (if (k < mb.mems.size - 1) "," else "") + "\n"
        }
        sb ++= "          ]\n"
        sb ++= "        }" + (if (j < ma.memBlocks.size - 1) "," else "") + "\n"
      }
      sb ++= "      ]\n"
      sb ++= "    }" + (if (i < sysMap.modules.size - 1) "," else "") + "\n"
    }
    sb ++= "  ]\n"
    sb ++= "}\n"
    sb.toString
  }
  private def q(s: String): String = "\"" + s.flatMap {
    case '"' => "\\\""; case '\\' => "\\\\"; case '\n' => "\\n"; case c => c.toString
  } + "\""
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}

/** 系统级 C 头文件：每模块基址宏 + 每寄存器/字段宏 */
object SystemCHeaderGen {
  def generate(sysMap: SystemMap): String = {
    val dev = sysMap.system.devName.toUpperCase
    val guard = s"${dev}_SYS_H"
    val sb = new StringBuilder
    sb ++= s"#ifndef $guard\n#define $guard\n\n"
    sb ++= s"/* ${sysMap.system.name} System Register Map - auto generated by RegCbb */\n\n"

    sysMap.modules.foreach { ma =>
      val mod = ma.module.name.toUpperCase
      sb ++= s"/* ============ Module: ${ma.module.name} @ ${hex(ma.baseAddress)} ============ */\n"
      sb ++= s"#define ${dev}_${mod}_BASE ${hex(ma.baseAddress)}\n"
      ma.regBlocks.foreach { rb =>
        rb.regs.foreach { a =>
          sb ++= s"/* ${a.reg.name} - ${a.reg.description} */\n"
          sb ++= s"#define ${dev}_${mod}_${a.reg.name.toUpperCase}_REG (${dev}_${mod}_BASE + ${hex(a.byteOffset)})\n"
          a.fieldAllocations.foreach { fa =>
            val f = fa.field
            val mask: String =
              if (f.bitWidth >= 64) s"0x${((BigInt(1) << f.bitWidth) - 1).toString(16)}"
              else s"0x${(((BigInt(1) << f.bitWidth) - 1) << fa.bitOffset).toString(16)}"
            sb ++= s"#define ${dev}_${mod}_${a.reg.name.toUpperCase}_${f.name.toUpperCase}_MASK  ${mask}\n"
            sb ++= s"#define ${dev}_${mod}_${a.reg.name.toUpperCase}_${f.name.toUpperCase}_SHIFT ${fa.bitOffset}\n"
            if (f.resetValue != 0)
              sb ++= s"#define ${dev}_${mod}_${a.reg.name.toUpperCase}_${f.name.toUpperCase}_RST  ${hex(f.resetValue)}\n"
          }
          sb ++= "\n"
        }
      }
      ma.memBlocks.foreach { mb =>
        mb.mems.foreach { m =>
          sb ++= s"/* ${m.mem.name} - ${m.mem.description} */\n"
          sb ++= s"#define ${dev}_${mod}_${m.mem.name.toUpperCase}_MEM   ${hex(m.baseAddress)}\n"
          sb ++= s"#define ${dev}_${mod}_${m.mem.name.toUpperCase}_DEPTH ${m.mem.depth}\n"
          sb ++= s"#define ${dev}_${mod}_${m.mem.name.toUpperCase}_WIDTH ${m.mem.dataWidth}\n"
          if (m.mem.entryFields.nonEmpty) {
            sb ++= s"/* ${m.mem.name} entry 域段： */\n"
            m.mem.entryFields.zip(m.mem.entryFieldOffsets).sortBy(-_._2).foreach { case (f, bitOffset) =>
              val mask: String =
                if (f.bitWidth >= 64) s"0x${((BigInt(1) << f.bitWidth) - 1).toString(16)}"
                else s"0x${(((BigInt(1) << f.bitWidth) - 1) << bitOffset).toString(16)}"
              sb ++= s"#define ${dev}_${mod}_${m.mem.name.toUpperCase}_${f.name.toUpperCase}_MASK  ${mask}\n"
              sb ++= s"#define ${dev}_${mod}_${m.mem.name.toUpperCase}_${f.name.toUpperCase}_SHIFT ${bitOffset}\n"
            }
          }
          sb ++= "\n"
        }
      }
      sb ++= "\n"
    }

    sb ++= s"#endif /* $guard */\n"
    sb.toString
  }
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}

/** 系统级 Markdown 手册：系统总览 + 每模块地址映射 + 每寄存器位域图 */
object SystemMarkdownGen {
  def generate(sysMap: SystemMap): String = {
    val s = sysMap.system
    val sb = new StringBuilder
    sb ++= s"# ${s.devName} 系统寄存器手册\n\n"
    sb ++= s"> 由 RegCbb 自动生成 · 系统 ${s.name} · 共 ${sysMap.modules.size} 个功能模块\n\n"
    if (s.description.nonEmpty) sb ++= s"${s.description}\n\n"

    sb ++= "## 系统地址总览\n\n"
    sb ++= "| 模块 | 寄存器基址 | 存储器基址 | 占用 | 说明 |\n|---|---|---|---|---|\n"
    sysMap.modules.foreach { ma =>
      sb ++= s"| `${ma.module.name}` | `${hex(ma.baseAddress)}` | `${hex(ma.memBaseAddress)}` | ${ma.sizeBytes} B | ${ma.module.description} |\n"
    }
    sb ++= "\n"

    sysMap.modules.foreach { ma =>
      sb ++= s"## 模块：${ma.module.name}（${hex(ma.baseAddress)}）\n\n"
      if (ma.module.description.nonEmpty) sb ++= s"${ma.module.description}\n\n"

      ma.regBlocks.foreach { rb =>
        sb ++= s"### 寄存器块：${rb.block.name}（${hex(rb.baseAddress)}）\n\n"
        sb ++= "| 偏移 | 名称 | 大小 | 访问/原子 | 复位值 | 说明 |\n|---|---|---|---|---|---|\n"
        rb.regs.foreach { a =>
          val atom = if (a.wordCount > 1) (if (a.reg.atomic) "多字·原子" else "多字·非原子") else "-"
          sb ++= s"| ${hex(a.byteOffset)} | `${a.reg.name}` | ${a.byteSize} B | $atom | ${hex(resetOf(a))} | ${a.reg.description} |\n"
        }
        sb ++= "\n"

        rb.regs.foreach { a =>
          sb ++= s"#### ${a.reg.name}（偏移 ${hex(a.byteOffset)}）\n\n"
          sb ++= "| 位 | 字段 | 访问 | 复位 | 描述 |\n|---|---|---|---|---|\n"
          a.fieldAllocations.sortBy(-_.bitOffset).foreach { fa =>
            val f = fa.field
            val bits = if (f.bitWidth == 1) s"[${fa.bitOffset}]" else s"[${fa.bitOffset + f.bitWidth - 1}:${fa.bitOffset}]"
            val enums = f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
              s"<br/>`0x${v.toString(16)}` = $n${if (d.nonEmpty) s"（$d）" else ""}"
            }.mkString
            sb ++= s"| $bits | `${f.name}` | ${f.access.id} | ${hex(f.resetValue)} | ${f.description}$enums |\n"
          }
          sb ++= "\n"
        }
      }

      if (ma.memBlocks.nonEmpty) {
        sb ++= s"### 存储器块\n\n"
        sb ++= "| 块 | 名称 | 基地址 | 深度 | 位宽 | 类型 | 原子 | 说明 |\n|---|---|---|---|---|---|---|---|\n"
        ma.memBlocks.foreach { mb =>
          mb.mems.foreach { m =>
            val atom = if (m.mem.wordCount > 1) (if (m.mem.atomic) "原子" else "非原子") else "-"
            sb ++= s"| `${mb.block.name}` | `${m.mem.name}` | ${hex(m.baseAddress)} | ${m.mem.depth} | ${m.mem.dataWidth} | ${m.mem.memType.id} | $atom | ${m.mem.description} |\n"
            if (m.mem.entryFields.nonEmpty) {
              sb ++= s"\n**${m.mem.name} entry 域段**（位宽 ${m.mem.dataWidth}，LSB-first）：\n\n"
              sb ++= "| 位 | 字段 | 访问 | 复位 | 描述 |\n|---|---|---|---|---|\n"
              m.mem.entryFields.zip(m.mem.entryFieldOffsets).sortBy(-_._2).foreach { case (f, bitOffset) =>
                val bits = if (f.bitWidth == 1) s"[${bitOffset}]" else s"[${bitOffset + f.bitWidth - 1}:${bitOffset}]"
                val enums = f.enumerations.toSeq.sortBy(_._1).map { case (v, (n, d)) =>
                  s"<br/>`0x${v.toString(16)}` = $n${if (d.nonEmpty) s"（$d）" else ""}"
                }.mkString
                sb ++= s"| $bits | `${f.name}` | ${f.access.id} | ${hex(f.resetValue)} | ${f.description}$enums |\n"
              }
              sb ++= "\n"
            }
          }
        }
        sb ++= "\n"
      }
    }

    sb ++= "---\n*本文档由 RegCbb 自动生成，请勿手工修改。*\n"
    sb.toString
  }
  private def resetOf(a: RegAllocation): BigInt =
    a.fieldAllocations.foldLeft(BigInt(0)) { case (acc, fa) => acc | (fa.field.resetValue << fa.bitOffset) }
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}

/** 系统级 HTML 手册（自包含，含侧边栏模块导航 + 模块寄存器一览表 + 锚点跳转） */
object SystemHtmlGen {
  def generate(sysMap: SystemMap): String = {
    val s = sysMap.system
    val sb = new StringBuilder
    sb ++= "<!DOCTYPE html>\n<html lang=\"zh-CN\">\n<head>\n<meta charset=\"utf-8\">\n"
    sb ++= s"<title>${esc(s.devName)} 系统寄存器手册</title>\n"
    sb ++= "<style>\n"
    sb ++= "html,body{margin:0;padding:0;height:100%;}\n"
    sb ++= "body{font-family:'Helvetica Neue',Arial,'PingFang SC','Microsoft YaHei',sans-serif;color:#222;display:flex;}\n"
    sb ++= "/* ===== 左侧侧边栏 ===== */\n"
    sb ++= "#sidebar{width:290px;min-width:290px;height:100vh;position:sticky;top:0;overflow-y:auto;\n"
    sb ++= "  background:#f5f6f8;border-right:1px solid #d8dbe0;padding:14px 10px;box-sizing:border-box;font-size:13px;}\n"
    sb ++= "#sidebar h1{font-size:15px;margin:0 0 10px 6px;border:none;padding:0;}\n"
    sb ++= "#sidebar a{color:#234;text-decoration:none;display:block;padding:2px 6px;border-radius:4px;}\n"
    sb ++= "#sidebar a:hover{background:#e4e8ee;}\n"
    sb ++= "#sidebar .mod a{font-weight:bold;font-size:13px;padding:4px 6px;}\n"
    sb ++= "#sidebar details{margin:2px 0;}\n"
    sb ++= "#sidebar summary{cursor:pointer;padding:3px 6px;border-radius:4px;font-weight:bold;color:#234;}\n"
    sb ++= "#sidebar summary:hover{background:#e4e8ee;}\n"
    sb ++= "#sidebar .blk{font-weight:bold;color:#456;margin:4px 0 2px 8px;}\n"
    sb ++= "#sidebar .blk a{color:#456;}\n"
    sb ++= "#sidebar ul{list-style:none;margin:0 0 6px 16px;padding:0;}\n"
    sb ++= "#sidebar li a{padding:1px 6px;color:#567;}\n"
    sb ++= "/* ===== 主内容区 ===== */\n"
    sb ++= "#main{flex:1;min-width:0;padding:24px 32px;box-sizing:border-box;}\n"
    sb ++= "h1{border-bottom:3px solid #345;padding-bottom:8px;}\n"
    sb ++= "h2{margin-top:32px;border-bottom:1px solid #ccc;padding-bottom:4px;}\n"
    sb ++= "h2:target,h3:target,h4:target{background:#fff7d6;border-radius:4px;padding-left:6px;}\n"
    sb ++= "table{border-collapse:collapse;margin:12px 0;font-size:14px;}\n"
    sb ++= "th,td{border:1px solid #bbb;padding:5px 10px;text-align:left;}\n"
    sb ++= "th{background:#eef1f5;}\n"
    sb ++= "code{background:#f4f4f4;padding:1px 4px;border-radius:3px;}\n"
    sb ++= ".meta{color:#666;font-size:13px;}\n"
    sb ++= ".reg-overview{font-size:13px;}\n"
    sb ++= "</style>\n</head>\n<body>\n"

    // ==================== 侧边栏 ====================
    sb ++= "<nav id=\"sidebar\">\n"
    sb ++= s"<h1>${esc(s.devName)}</h1>\n"
    sb ++= "<a href=\"#top\">☰ 系统地址总览</a>\n"
    sysMap.modules.foreach { ma =>
      val modId = anchor(ma.module.name)
      sb ++= s"<details open><summary>模块 ${esc(ma.module.name)}</summary>\n"
      sb ++= s"<a href=\"#mod_$modId\">↗ ${hex(ma.baseAddress)} 模块寄存器一览</a>\n"
      ma.regBlocks.foreach { rb =>
        sb ++= s"<div class=\"blk\"><a href=\"#blk_${modId}_${anchor(rb.block.name)}\">${esc(rb.block.name)}</a></div>\n"
        sb ++= "<ul>\n"
        rb.regs.foreach { a =>
          sb ++= s"<li><a href=\"#reg_${modId}_${anchor(a.reg.name)}\">${esc(a.reg.name)}</a></li>\n"
        }
        sb ++= "</ul>\n"
      }
      if (ma.memBlocks.nonEmpty) {
        sb ++= "<div class=\"blk\">存储器</div>\n<ul>\n"
        ma.memBlocks.foreach { mb =>
          mb.mems.foreach { m =>
            sb ++= s"<li><a href=\"#mem_${modId}_${anchor(m.mem.name)}\">${esc(m.mem.name)}</a></li>\n"
          }
        }
        sb ++= "</ul>\n"
      }
      sb ++= "</details>\n"
    }
    sb ++= "</nav>\n"

    // ==================== 主内容 ====================
    sb ++= "<main id=\"main\">\n"
    sb ++= s"<h1 id=\"top\">${esc(s.devName)} 系统寄存器手册</h1>\n"
    sb ++= s"<p class=\"meta\">由 RegCbb 自动生成 · 系统 ${esc(s.name)} · 共 ${sysMap.modules.size} 个功能模块</p>\n"
    if (s.description.nonEmpty) sb ++= s"<p>${esc(s.description)}</p>\n"

    sb ++= "<h2>系统地址总览</h2>\n<table><tr><th>模块</th><th>寄存器基址</th><th>存储器基址</th><th>占用</th><th>说明</th></tr>\n"
    sysMap.modules.foreach { ma =>
      sb ++= s"<tr><td><a href=\"#mod_${anchor(ma.module.name)}\"><code>${esc(ma.module.name)}</code></a></td>" +
        s"<td><code>${hex(ma.baseAddress)}</code></td><td><code>${hex(ma.memBaseAddress)}</code></td>" +
        s"<td>${ma.sizeBytes} B</td><td>${esc(ma.module.description)}</td></tr>\n"
    }
    sb ++= "</table>\n"

    sysMap.modules.foreach { ma =>
      val modId = anchor(ma.module.name)
      sb ++= s"<h2 id=\"mod_$modId\">模块 ${esc(ma.module.name)}（<code>${hex(ma.baseAddress)}</code>）</h2>\n"

      // ---- 模块寄存器一览表（所有寄存器：名称/地址/功能概述，可跳转） ----
      sb ++= "<h3>寄存器一览</h3>\n"
      sb ++= "<table class=\"reg-overview\"><tr><th>寄存器</th><th>绝对地址</th><th>大小</th><th>访问/原子</th><th>复位值</th><th>功能概述</th></tr>\n"
      ma.regBlocks.foreach { rb =>
        rb.regs.foreach { a =>
          val atom = if (a.wordCount > 1) (if (a.reg.atomic) "多字·原子" else "多字·非原子") else "-"
          sb ++= s"<tr><td><a href=\"#reg_${modId}_${anchor(a.reg.name)}\"><code>${esc(a.reg.name)}</code></a></td>" +
            s"<td><code>${hex(ma.baseAddress + a.byteOffset)}</code></td><td>${a.byteSize} B</td><td>$atom</td>" +
            s"<td><code>${hex(resetOf(a))}</code></td><td>${esc(a.reg.description)}</td></tr>\n"
        }
      }
      sb ++= "</table>\n"

      // ---- 各寄存器块详细 ----
      ma.regBlocks.foreach { rb =>
        sb ++= s"<h3 id=\"blk_${modId}_${anchor(rb.block.name)}\">寄存器块 ${esc(rb.block.name)}（<code>${hex(rb.baseAddress)}</code>）</h3>\n"
        rb.regs.foreach { a =>
          sb ++= s"<h4 id=\"reg_${modId}_${anchor(a.reg.name)}\">${esc(a.reg.name)}（偏移 ${hex(a.byteOffset)}）</h4>\n"
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
      }
      if (ma.memBlocks.nonEmpty) {
        sb ++= "<h3>存储器块</h3>\n<table><tr><th>块</th><th>名称</th><th>基地址</th><th>深度</th><th>位宽</th><th>类型</th><th>原子</th><th>说明</th></tr>\n"
        ma.memBlocks.foreach { mb =>
          mb.mems.foreach { m =>
            val atom = if (m.mem.wordCount > 1) (if (m.mem.atomic) "原子" else "非原子") else "-"
            sb ++= s"<tr><td><code>${esc(mb.block.name)}</code></td>" +
              s"<td><a href=\"#mem_${modId}_${anchor(m.mem.name)}\"><code>${esc(m.mem.name)}</code></a></td>" +
              s"<td><code>${hex(m.baseAddress)}</code></td><td>${m.mem.depth}</td><td>${m.mem.dataWidth}</td>" +
              s"<td>${m.mem.memType.id}</td><td>$atom</td><td>${esc(m.mem.description)}</td></tr>\n"
            if (m.mem.entryFields.nonEmpty) {
              sb ++= s"<tr><td colspan=\"8\"><b>${esc(m.mem.name)} entry 域段</b>（位宽 ${m.mem.dataWidth}，LSB-first）：" +
                "<table><tr><th>位</th><th>字段</th><th>访问</th><th>复位</th><th>描述</th></tr>\n"
              m.mem.entryFields.zip(m.mem.entryFieldOffsets).sortBy(-_._2).foreach { case (f, bitOffset) =>
                val bits = if (f.bitWidth == 1) s"[${bitOffset}]" else s"[${bitOffset + f.bitWidth - 1}:${bitOffset}]"
                sb ++= s"<tr><td>$bits</td><td><code>${esc(f.name)}</code></td><td>${f.access.id}</td>" +
                  s"<td><code>${hex(f.resetValue)}</code></td><td>${esc(f.description)}</td></tr>\n"
              }
              sb ++= "</table></td></tr>\n"
            }
          }
        }
        sb ++= "</table>\n"
        // 存储器详细锚点（供侧边栏跳转；详细内容在下方补充）
        ma.memBlocks.foreach { mb =>
          mb.mems.foreach { m =>
            sb ++= s"<h4 id=\"mem_${modId}_${anchor(m.mem.name)}\">${esc(m.mem.name)}（${hex(m.baseAddress)}，${m.mem.depth}×${m.mem.dataWidth}）</h4>\n"
          }
        }
      }
    }

    sb ++= "<hr/><p class=\"meta\">本文档由 RegCbb 自动生成，请勿手工修改。</p>\n"
    sb ++= "</main>\n"
    sb ++= "</body>\n</html>\n"
    sb.toString
  }
  /** 锚点 id 清洗：非字母数字下划线 → _，避免 HTML id 冲突/非法 */
  private def anchor(s: String): String =
    if (s.isEmpty) "_" else s.map(c => if (c.isLetterOrDigit || c == '_') c else '_')
  private def resetOf(a: RegAllocation): BigInt =
    a.fieldAllocations.foldLeft(BigInt(0)) { case (acc, fa) => acc | (fa.field.resetValue << fa.bitOffset) }
  private def esc(s: String): String = s.flatMap {
    case '&' => "&amp;"; case '<' => "&lt;"; case '>' => "&gt;"; case '"' => "&quot;"; case c => c.toString
  }
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}

/** 系统级具名视图源码生成器：模块 → 块 → 寄存器 三级编译期具名访问 */
object SystemViewSourceGen {
  def generate(sysMap: SystemMap, className: String, packageName: String): String = {
    val sb = new StringBuilder
    sb ++= s"package $packageName\n\n"
    sb ++= "import BaseCbb.RegCbb.hw._\n\n"
    sb ++= s"/** 由 RegCbb 自动生成：系统级编译期具名寄存器视图 */\n"
    sb ++= s"class $className(view: SystemRegView) {\n"
    sysMap.modules.foreach { ma =>
      val modId = sanitize(ma.module.name)
      sb ++= s"  /** 模块 ${ma.module.name} @ ${hex(ma.baseAddress)} */\n"
      sb ++= s"  val $modId = view.module(\"${ma.module.name}\")\n"
      ma.regBlocks.foreach { rb =>
        val blkId = sanitize(rb.block.name)
        sb ++= s"  /** 寄存器块 ${rb.block.name} */\n"
        sb ++= s"  val $modId${blkId.capitalize} = view.module(\"${ma.module.name}\").block(\"${rb.block.name}\")\n"
      }
      sb ++= "\n"
    }
    sb ++= "}\n"
    sb.toString
  }
  private def sanitize(s: String): String =
    if (s.isEmpty) "_" else s.map(c => if (c.isLetterOrDigit || c == '_') c else '_')
  private def hex(v: BigInt): String = s"0x${v.toString(16)}"
}
