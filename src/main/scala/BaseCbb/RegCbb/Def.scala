package BaseCbb.RegCbb

/**
 * RegCbb_v2 核心 IR（中间表示）—— 单一事实源。
 *
 * 一次定义，同时驱动：
 *   1. RTL 生成（hw.RegFileTop / hw.AxiLiteRegFile）
 *   2. 用户逻辑连接视图（hw.RegView + 生成的具名视图类）
 *   3. 文档与软件视图（gen.JsonGen / gen.CHeaderGen / gen.MarkdownGen）
 */

/** 字段访问类型（软件视角 + 硬件语义） */
sealed trait AccessType {
  def id: String
  /** 软件可读 */
  def swReadable: Boolean
  /** 软件可写 */
  def swWritable: Boolean
  /** 读是否产生副作用（RC 清零 / RS 全置 1） */
  def hasReadEffect: Boolean = this == AccessType.RC || this == AccessType.RS
  /** 硬件是否可置位/清除/翻转（W1C/RC 可置位，W1S/RS 可清除，W1T 可翻转） */
  def hwAction: Option[HwAction] = this match {
    case AccessType.W1C | AccessType.RC => Some(HwAction.Set)
    case AccessType.W1S | AccessType.RS => Some(HwAction.Clear)
    case AccessType.W1T                 => Some(HwAction.Toggle)
    case _                              => None
  }
}
object AccessType {
  case object RW  extends AccessType { val id = "RW";  val swReadable = true;  val swWritable = true  }
  case object RO  extends AccessType { val id = "RO";  val swReadable = true;  val swWritable = false }
  case object WO  extends AccessType { val id = "WO";  val swReadable = false; val swWritable = true  }
  case object RC  extends AccessType { val id = "RC";  val swReadable = true;  val swWritable = true  }
  case object RS  extends AccessType { val id = "RS";  val swReadable = true;  val swWritable = true  }
  case object W1C extends AccessType { val id = "W1C"; val swReadable = true;  val swWritable = true  }
  case object W1S extends AccessType { val id = "W1S"; val swReadable = true;  val swWritable = true  }
  case object W1T extends AccessType { val id = "W1T"; val swReadable = true;  val swWritable = true  }

  val all: Seq[AccessType] = Seq(RW, RO, WO, RC, RS, W1C, W1S, W1T)
  def fromId(s: String): AccessType =
    all.find(_.id == s).getOrElse(sys.error(s"unknown access type: $s"))
}

/** 硬件对字段的写入动作（HW → 寄存器） */
sealed trait HwAction { def id: String }
object HwAction {
  case object Set    extends HwAction { val id = "Set"    } // 或 1 置位（W1C/RC）
  case object Clear  extends HwAction { val id = "Clear"  } // 与 0 清除（W1S/RS）
  case object Toggle extends HwAction { val id = "Toggle" } // 异或翻转（W1T）
}

/** 软件写动作（文档用；硬件语义由 AccessType 决定） */
sealed trait WriteAction { def id: String }
object WriteAction {
  case object Normal      extends WriteAction { val id = "Normal"      }
  case object OneToClear  extends WriteAction { val id = "OneToClear"  }
  case object OneToSet    extends WriteAction { val id = "OneToSet"    }
  case object OneToToggle extends WriteAction { val id = "OneToToggle" }
  case object ClearOnRead extends WriteAction { val id = "ClearOnRead" }
}

/** 存储器类型 */
sealed trait MemoryAccessType { def id: String }
object MemoryAccessType {
  case object SP extends MemoryAccessType { val id = "SP" } // 单口 SRAM
  case object TP extends MemoryAccessType { val id = "TP" } // 双口 SRAM
}

/** 字段定义（位宽、访问类型、复位值、描述、枚举 —— 文档与 RTL 的全部元数据） */
case class RegFieldDef(
  name: String,
  bitWidth: Int,
  access: AccessType = AccessType.RW,
  resetValue: BigInt = 0,
  description: String = "",
  writeAction: WriteAction = WriteAction.Normal,
  enumerations: Map[BigInt, (String, String)] = Map.empty
) {
  require(bitWidth > 0 && bitWidth <= 256, s"field '$name': bitWidth must be in 1..256, got $bitWidth")
  require(resetValue >= 0, s"field '$name': resetValue must be >= 0")
  if (bitWidth <= 64)
    require(resetValue < (BigInt(1) << bitWidth),
      s"field '$name': resetValue $resetValue out of range for $bitWidth bits")
}

/** 寄存器定义（字段集合；地址偏移由 AddressAllocator 分配） */
case class RegDef(
  name: String,
  fields: Seq[RegFieldDef],
  description: String = "",
  group: Option[String] = None,
  /** 多字（>总线位宽）寄存器是否原子：原子=写低字暂存、写最高字一次提交；非原子=逐字直接写 */
  atomic: Boolean = true
) {
  require(fields.nonEmpty, s"register '$name': need at least one field")
  require(fields.map(_.name).distinct.size == fields.size,
    s"register '$name': duplicate field names: ${fields.map(_.name).distinct.mkString(",")}")
  val totalBits: Int = fields.map(_.bitWidth).sum

  /**
   * 占据位宽（规则 3）：
   *  - totalBits <= 32：占据 32bit（1 word），有效数据右对齐在低 bit 位（bit[0..totalBits-1]）；
   *  - totalBits > 32：占据能容纳其位宽的 **2 的幂** 宽度（如 40→64、96→128），
   *    有效数据从高 bit 位开始放（低 bit 位为 padding 0）。
   */
  val expandedBits: Int = if (totalBits <= 32) 32 else {
    var p = 32
    while (p < totalBits) p <<= 1
    p
  }
  /** 有效数据起始 bit：<=32bit 右对齐（0）；>32bit 高 bit 放置（expandedBits - totalBits） */
  val fieldBaseOffset: Int = if (totalBits <= 32) 0 else expandedBits - totalBits
  /** 占用的 32bit word 数 */
  val wordCount: Int = expandedBits / 32
  /** 字节大小（按 4 字节对齐） */
  val byteSize: Int = wordCount * 4
}

/** 存储器定义（一片连续地址空间，位宽可为总线位宽的整数倍） */
case class MemoryDef(
  name: String,
  depth: Int,
  dataWidth: Int,
  memType: MemoryAccessType = MemoryAccessType.SP,
  /** 手工指定基地址；None = 由 AddressAllocator 从块 memBaseAddress 起自动分配 */
  baseAddress: Option[BigInt] = None,
  description: String = "",
  /** 总线访问（位宽 < memory 位宽时）是否原子：原子=写低字暂存、写最高字一次提交；非原子=逐字读-改-写 */
  atomic: Boolean = true,
  /** entry 域段信息（可选）：每个 entry 的字段布局（LSB-first 紧凑）；位宽和必须 == dataWidth */
  entryFields: Seq[RegFieldDef] = Seq.empty
) {
  require(depth > 0, s"memory '$name': depth must be > 0")
  require(dataWidth >= 32 && dataWidth % 32 == 0 && dataWidth <= 256,
    s"memory '$name': dataWidth must be a multiple of 32 in [32,256]")
  require(baseAddress.forall(_ >= 0), s"memory '$name': baseAddress must be >= 0")
  require(entryFields.map(_.name).distinct.size == entryFields.size,
    s"memory '$name': duplicate entry field names: ${entryFields.map(_.name).distinct.mkString(",")}")
  require(entryFields.isEmpty || entryFields.map(_.bitWidth).sum == dataWidth,
    s"memory '$name': entryFields total width ${entryFields.map(_.bitWidth).sum} != dataWidth $dataWidth")
  /** 地址位宽 = ceil(log2(depth))（按 dataWidth 单元编址） */
  val addrWidth: Int = {
    var w = 0; var d = depth - 1
    while (d > 0) { d >>= 1; w += 1 }
    math.max(1, w)
  }

  /**
   * 占据位宽（规则 3）：
   *  - dataWidth <= 32：占据 32bit（1 word），有效数据右对齐在低 bit 位；
   *  - dataWidth > 32：占据能容纳其位宽的 **2 的幂** 宽度（如 96→128），
   *    有效数据从高 bit 位开始放（低 bit 位为 padding 0）。
   *
   * 说明：MemPortIO 外部接口宽度保持用户定义 dataWidth（外部 SRAM 位宽不变），
   * 占据地址空间（byteSize/wordCount）按 2 的幂扩展；entry 域段在占据空间内高 bit 放置。
   */
  val expandedDataWidth: Int = if (dataWidth <= 32) 32 else {
    var p = 32
    while (p < dataWidth) p <<= 1
    p
  }
  /** 有效数据起始 bit：<=32bit 右对齐（0）；>32bit 高 bit 放置（expandedDataWidth - dataWidth） */
  val dataBaseOffset: Int = if (dataWidth <= 32) 0 else expandedDataWidth - dataWidth
  /** 占据的 32bit word 数（按 2 的幂扩展；决定地址空间大小） */
  val wordCount: Int = expandedDataWidth / 32
  /** 字节大小（按占据位宽；地址空间 2 的幂对齐） */
  val byteSize: BigInt = BigInt(depth) * BigInt(expandedDataWidth) / 8
  /** entry 字段位偏移（基于占据位宽的高 bit 放置 + LSB-first 紧凑） */
  val entryFieldOffsets: Seq[Int] =
    entryFields.scanLeft(dataBaseOffset)(_ + _.bitWidth).init
}

object MemoryDef {
  /** 便捷工厂：entry 域段来自 RegBundle 转换的字段序列，dataWidth 自动取字段位宽和 */
  def fromBundle(name: String, depth: Int, fields: Seq[RegFieldDef],
                 memType: MemoryAccessType = MemoryAccessType.SP,
                 baseAddress: Option[BigInt] = None,
                 description: String = "",
                 atomic: Boolean = true): MemoryDef =
    MemoryDef(name, depth, fields.map(_.bitWidth).sum, memType, baseAddress, description, atomic, fields)
}

/** 寄存器块定义（纯寄存器集合 —— 一个功能片段，不含存储器） */
case class RegBlockDef(
  name: String,
  registers: Seq[RegDef],
  description: String = ""
) {
  require(registers.nonEmpty, s"reg block '$name': need at least one register")
  require(registers.map(_.name).distinct.size == registers.size,
    s"reg block '$name': duplicate register names")
  val byteSize: BigInt = registers.map(_.byteSize).sum
}

/** 存储器块定义（纯存储器集合 —— 与寄存器块分离的另一种 RegBlock） */
case class MemBlockDef(
  name: String,
  memories: Seq[MemoryDef],
  description: String = ""
) {
  require(memories.nonEmpty, s"mem block '$name': need at least one memory")
  require(memories.map(_.name).distinct.size == memories.size,
    s"mem block '$name': duplicate memory names")
  /** 存储器空间大小（按字节；含内部空隙的近似，按各 memory byteSize 求和） */
  val byteSize: BigInt = memories.map(_.byteSize).sum
}

/**
 * 功能模块定义：一个功能模块 = 多个寄存器块（RegBlockDef）+ 多个存储器块（MemBlockDef）。
 *  - `baseAddress`：模块寄存器区基址。None = 由 System 分配器自动分配；Some = 手工指定（自动分配会跳过已占用区域）。
 *  - `memBaseAddress`：模块存储器区基址。None = 自动紧随寄存器区之后（4 字节对齐）；Some = 手工指定。
 *  - 模块内寄存器块连续排布（字节 4 对齐），存储器块区紧随寄存器区之后。
 */
case class ModuleDef(
  name: String,
  regBlocks: Seq[RegBlockDef] = Seq.empty,
  memBlocks: Seq[MemBlockDef] = Seq.empty,
  baseAddress: Option[BigInt] = None,
  memBaseAddress: Option[BigInt] = None,
  description: String = ""
) {
  require(regBlocks.nonEmpty || memBlocks.nonEmpty,
    s"module '$name': need at least one reg block or mem block")
  require(regBlocks.map(_.name).distinct.size == regBlocks.size,
    s"module '$name': duplicate reg block names")
  require(memBlocks.map(_.name).distinct.size == memBlocks.size,
    s"module '$name': duplicate mem block names")
  def regByteSize: BigInt = regBlocks.map(_.byteSize).sum
  def memByteSize: BigInt = memBlocks.map(_.byteSize).sum
  def allRegisters: Seq[RegDef] = regBlocks.flatMap(_.registers)
  def allMemories: Seq[MemoryDef] = memBlocks.flatMap(_.memories)
}

/** 系统定义：一个系统 = 多个功能模块，模块间地址自动/手工分配，系统级译码分发 */
case class SystemDef(
  name: String,
  modules: Seq[ModuleDef],
  deviceName: String = "",
  description: String = ""
) {
  require(modules.map(_.name).distinct.size == modules.size,
    s"system '$name': duplicate module names")
  def devName: String = if (deviceName.nonEmpty) deviceName else name
  def allRegisters: Seq[RegDef] = modules.flatMap(_.allRegisters)
  def allMemories: Seq[MemoryDef] = modules.flatMap(_.allMemories)
}
