package BaseCbb.RegCbb

import scala.collection.mutable.ArrayBuffer

/** 字段在寄存器内的位分配 */
case class FieldAllocation(field: RegFieldDef, bitOffset: Int) {
  def hi: Int = bitOffset + field.bitWidth - 1
}

/** 寄存器分配（byteOffset 为相对所属映射 regBaseAddress 的偏移） */
case class RegAllocation(
  reg: RegDef,
  byteOffset: BigInt,
  fieldAllocations: Seq[FieldAllocation]
) {
  def totalBits: Int = reg.totalBits
  def wordCount: Int = reg.wordCount
  def byteSize: Int = reg.byteSize
}

/** 存储器分配（baseAddress 为绝对字节地址） */
case class MemAllocation(mem: MemoryDef, baseAddress: BigInt)

/**
 * 完整地址映射（单块 / 单模块视图）：
 *  - regBaseAddress：寄存器区基址（绝对地址），寄存器命中 = regBaseAddress + reg.byteOffset
 *  - memBaseAddress：存储器区基址（绝对地址），存储器命中 = mem.baseAddress
 * 供 RTL（RegFileTop）/ 文档 / 软件视图共同使用。
 */
case class RegFileMap(
  blockName: String,
  deviceName: String,
  description: String,
  regBaseAddress: BigInt,
  memBaseAddress: BigInt,
  regs: Seq[RegAllocation],
  mems: Seq[MemAllocation]
) {
  def totalRegByteSize: BigInt = if (regs.isEmpty) 0 else regs.map(r => r.byteOffset + r.byteSize).max
  def totalMemByteSize: BigInt = if (mems.isEmpty) 0 else mems.map(m => m.baseAddress + m.mem.byteSize).max - memBaseAddress
  def regByName(name: String): RegAllocation =
    regs.find(_.reg.name == name).getOrElse(sys.error(s"register '$name' not found"))
}

// ==================== 系统级分配结果 ====================

/** 寄存器块分配：块基址 + 块内寄存器（byteOffset 相对模块基址） */
case class RegBlockAllocation(block: RegBlockDef, baseAddress: BigInt, regs: Seq[RegAllocation])

/** 存储器块分配：块基址（绝对）+ 块内存储器 */
case class MemBlockAllocation(block: MemBlockDef, baseAddress: BigInt, mems: Seq[MemAllocation])

/** 模块分配：模块基址 + 各寄存器块/存储器块 */
case class ModuleAllocation(
  module: ModuleDef,
  baseAddress: BigInt,
  regBlocks: Seq[RegBlockAllocation],
  memBlocks: Seq[MemBlockAllocation],
  memBaseAddress: BigInt
) {
  /** 寄存器区大小 */
  def regByteSize: BigInt = module.regByteSize
  /** 存储器区大小 */
  def memByteSize: BigInt = module.memByteSize
  /** 模块总占用字节（寄存器区 + 存储器区，均计入） */
  def sizeBytes: BigInt =
    if (memBlocks.isEmpty) regByteSize
    else (memBaseAddress + memByteSize) - baseAddress
  /** 模块内全部寄存器（byteOffset 相对模块基址） */
  def allRegs: Seq[RegAllocation] = regBlocks.flatMap(_.regs)
  /** 模块内全部存储器（绝对基址） */
  def allMems: Seq[MemAllocation] = memBlocks.flatMap(_.mems)

  /** 模块级 RegFileMap：regBaseAddress = 模块基址；供模块 RegFileTop / 单模块文档 */
  def toRegFileMap: RegFileMap =
    RegFileMap(module.name, module.name, module.description, baseAddress,
      memBaseAddress, allRegs, allMems)
}

/** 系统分配：完整系统地址映射（供 SystemRegFileTop / SystemRegView / 系统文档） */
case class SystemMap(
  system: SystemDef,
  modules: Seq[ModuleAllocation]
) {
  /** 平铺全系统寄存器（byteOffset 为绝对字节地址，供系统级视图/文档） */
  def allRegsAbsolute: Seq[RegAllocation] =
    modules.flatMap(ma => ma.allRegs.map(ra => ra.copy(byteOffset = ma.baseAddress + ra.byteOffset)))
  /** 平铺全系统存储器（绝对基址） */
  def allMemsAbsolute: Seq[MemAllocation] = modules.flatMap(_.allMems)

  /** 平铺系统级 RegFileMap（regBaseAddress=0，byteOffset=绝对地址；供 io.user / 系统文档） */
  def flatMap: RegFileMap =
    RegFileMap(system.name, system.devName, system.description, 0, 0,
      allRegsAbsolute, allMemsAbsolute)

  def moduleByName(name: String): ModuleAllocation =
    modules.find(_.module.name == name).getOrElse(sys.error(s"module '$name' not found"))
  def regByName(name: String): RegAllocation =
    allRegsAbsolute.find(_.reg.name == name).getOrElse(sys.error(s"register '$name' not found"))

  /** 地址映射摘要（调试） */
  def summarize: String = {
    val sb = new StringBuilder
    sb ++= s"System : ${system.name}\n"
    sb ++= s"Device : ${system.devName}\n"
    modules.foreach { ma =>
      sb ++= s"\n=== Module: ${ma.module.name} @ 0x${ma.baseAddress.toString(16)} (${ma.sizeBytes} B) ===\n"
      ma.regBlocks.foreach { rb =>
        sb ++= s"  RegBlock: ${rb.block.name} @ 0x${rb.baseAddress.toString(16)}\n"
        rb.regs.sortBy(_.byteOffset).foreach { a =>
          sb ++= s"    ${a.reg.name} @ 0x${(ma.baseAddress + a.byteOffset).toString(16)} (${a.byteSize}B, ${a.wordCount} word)\n"
        }
      }
      ma.memBlocks.foreach { mb =>
        sb ++= s"  MemBlock: ${mb.block.name} @ 0x${mb.baseAddress.toString(16)}\n"
        mb.mems.foreach { m =>
          sb ++= s"    ${m.mem.name} @ 0x${m.baseAddress.toString(16)} (${m.mem.depth}x${m.mem.dataWidth}, ${m.mem.byteSize}B)\n"
        }
      }
    }
    sb.toString
  }
}

/**
 * 地址分配器。
 *
 * 两级分配：
 *  1. 块级（allocateRegBlock/allocateMemBlock）：字段 LSB-first、寄存器 32bit word 对齐；
 *  2. 系统级（allocateSystem）：模块基址自动（从 moduleBaseAddress 起，跳过已占用）/手工（baseAddress），
 *     模块内寄存器块连续排布（4 字节对齐），存储器块区紧随寄存器区之后。
 */
object AddressAllocator {

  /** 单寄存器块 → RegAllocation 序列（相对 offset 起点 0；字段从 reg.fieldBaseOffset 起，LSB-first 紧凑） */
  def allocateRegisters(registers: Seq[RegDef]): Seq[RegAllocation] = {
    var off = BigInt(0)
    registers.map { r =>
      var bit = r.fieldBaseOffset   // 规则 3：>32bit 从高 bit 位开始放；<=32bit 右对齐（0）
      val fas = r.fields.map { f =>
        val fa = FieldAllocation(f, bit)
        bit += f.bitWidth
        fa
      }
      val ra = RegAllocation(r, off, fas)
      off += r.byteSize
      ra
    }
  }

  /** 系统级分配（自动/手工模块基址） */
  def allocateSystem(sys: SystemDef, moduleBaseAddress: BigInt = 0): SystemMap = {
    var next = moduleBaseAddress
    val mods = sys.modules.map { m =>
      val base = m.baseAddress.getOrElse(next)

      // ---- 寄存器区：各寄存器块连续排布 ----
      var regOff = BigInt(0)
      val regBlocks = m.regBlocks.map { rb =>
        val rbBase = base + regOff
        val regs = allocateRegisters(rb.registers).map(ra => ra.copy(byteOffset = regOff + ra.byteOffset))
        regOff += rb.byteSize
        RegBlockAllocation(rb, rbBase, regs)
      }

      // ---- 存储器区：模块 memBaseAddress（手工）或寄存器区之后 4 字节对齐（自动）----
      var memOff = m.memBaseAddress.getOrElse(align4(base + regOff))
      val memBlocks = m.memBlocks.map { mb =>
        val mbBase = memOff
        val mems = mb.memories.map { mem =>
          val ma = MemAllocation(mem, memOff)
          memOff += mem.byteSize
          ma
        }
        MemBlockAllocation(mb, mbBase, mems)
      }

      val end = align4(memOff)
      if (end > next) next = end // 跳过已占用区域（含手工指定模块）
      ModuleAllocation(m, base, regBlocks, memBlocks, m.memBaseAddress.getOrElse(align4(base + regOff)))
    }
    SystemMap(sys, mods)
  }

  /** 便捷：单模块（如无系统概念时）直接分配 */
  def allocateModule(m: ModuleDef): ModuleAllocation =
    allocateSystem(SystemDef(m.name, Seq(m), m.name)).modules.head

  /** 便捷：单个寄存器块（纯寄存器，无存储器）→ 单模块分配 */
  def allocate(block: RegBlockDef, regBaseAddress: BigInt = 0): RegFileMap = {
    val mod = ModuleDef(block.name, Seq(block), Seq.empty, Some(regBaseAddress), None, block.description)
    val ma = allocateModule(mod)
    ma.toRegFileMap
  }

  /** 兼容便捷：旧式"寄存器+存储器混合块"→ 单模块（迁移期使用，推荐改用 RegBlock/MemBlock 分离） */
  def allocate(registers: Seq[RegDef], memories: Seq[MemoryDef],
               regBaseAddress: BigInt, memBaseAddress: BigInt,
               name: String, deviceName: String, description: String): RegFileMap = {
    val regBlock = RegBlockDef(name + "_regs", registers, description)
    val memBlock = if (memories.nonEmpty) Seq(MemBlockDef(name + "_mems", memories, description)) else Seq.empty
    val mod = ModuleDef(name, Seq(regBlock), memBlock, Some(regBaseAddress), None, description)
    val ma = allocateModule(mod)
    ma.toRegFileMap.copy(blockName = name, deviceName = deviceName)
  }

  private def align4(v: BigInt): BigInt = ((v + 3) / 4) * 4

  /** 单块摘要（RegFileMap，调试用） */
  def summarize(map: RegFileMap): String = {
    val sb = new StringBuilder
    sb ++= s"Register Block : ${map.blockName}\n"
    sb ++= s"Device         : ${map.deviceName}\n"
    sb ++= s"Reg  Base      : 0x${map.regBaseAddress.toString(16)}\n"
    sb ++= s"Mem  Base      : 0x${map.memBaseAddress.toString(16)}\n"
    sb ++= s"Reg  Space     : ${map.totalRegByteSize} bytes\n"
    sb ++= s"Mem  Space     : ${map.totalMemByteSize} bytes\n\n"
    sb ++= "=== Registers ===\n"
    map.regs.foreach { a =>
      sb ++= s"  ${a.reg.name} @ 0x${(map.regBaseAddress + a.byteOffset).toString(16)} (${a.byteSize}B, ${a.wordCount} word)\n"
      a.fieldAllocations.sortBy(_.bitOffset).foreach { fa =>
        val f = fa.field
        val bits = if (f.bitWidth == 1) s"[${fa.bitOffset}]" else s"[${fa.bitOffset + f.bitWidth - 1}:${fa.bitOffset}]"
        sb ++= s"    $bits ${f.name} (${f.access.id}, reset=${f.resetValue}) ${f.description}\n"
      }
    }
    if (map.mems.nonEmpty) {
      sb ++= "\n=== Memories ===\n"
      map.mems.foreach { m =>
        sb ++= s"  ${m.mem.name} @ 0x${m.baseAddress.toString(16)} (${m.mem.depth}x${m.mem.dataWidth}, ${m.mem.byteSize}B)\n"
      }
    }
    sb.toString
  }
}
