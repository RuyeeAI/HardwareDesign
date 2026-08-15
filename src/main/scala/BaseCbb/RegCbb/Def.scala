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
  /** 占用的 32bit word 数 */
  val wordCount: Int = (totalBits + 31) / 32
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
  atomic: Boolean = true
) {
  require(depth > 0, s"memory '$name': depth must be > 0")
  require(dataWidth >= 32 && dataWidth % 32 == 0 && dataWidth <= 256,
    s"memory '$name': dataWidth must be a multiple of 32 in [32,256]")
  require(baseAddress.forall(_ >= 0), s"memory '$name': baseAddress must be >= 0")
  /** 地址位宽 = ceil(log2(depth))（按 dataWidth 单元编址） */
  val addrWidth: Int = {
    var w = 0; var d = depth - 1
    while (d > 0) { d >>= 1; w += 1 }
    math.max(1, w)
  }
  /** 总线 32bit word 数 */
  val wordCount: Int = dataWidth / 32
  val byteSize: BigInt = BigInt(depth) * BigInt(dataWidth) / 8
}

/** 寄存器块定义（一个外设的完整寄存器文件） */
case class RegBlockDef(
  name: String,
  regBaseAddress: BigInt,
  memBaseAddress: BigInt,
  registers: Seq[RegDef],
  memories: Seq[MemoryDef] = Seq.empty,
  description: String = "",
  deviceName: String = ""
) {
  require(registers.map(_.name).distinct.size == registers.size,
    s"block '$name': duplicate register names")
  def devName: String = if (deviceName.nonEmpty) deviceName else name
}
