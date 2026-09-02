package BaseCbb.RegCbb.hw

import chisel3._
import chisel3.util.{MuxCase, MuxLookup, RegEnable, log2Ceil, switch, is, Enum}
import BaseCbb.RegCbb._
import scala.collection.immutable.ListMap

/** 总线侧接口（dataWidth 通常为 32，单拍读写） */
class DecIO(val dataWidth: Int) extends Bundle {
  val wr    = Input(Bool())
  val wdata = Input(UInt(dataWidth.W))
  val rd    = Input(Bool())
  val rdata = Output(UInt(dataWidth.W))
}

/** 按字段名的硬件输入集合（每元素宽度 = 字段位宽；元素即为可赋值的真实端口） */
class FieldInputRecord(entries: Seq[(String, Int)]) extends Record {
  val elements: ListMap[String, UInt] =
    ListMap(entries.map { case (n, w) => n -> Input(UInt(w.W)) }: _*)

  /** 按字段名取输入端口（该类型无此字段时给出可用字段清单） */
  def apply(fname: String): UInt =
    elements.getOrElse(fname,
      sys.error(s"field '$fname' not found, available: ${elements.keys.mkString(", ")}"))
}

// ============================================================================
// 按访问类型划分的用户侧接口 Bundle
// ============================================================================

/** SW→HW 事件视图（寄存器 → 用户逻辑，Output）：
  * wrEn/wrData 写脉冲与数据（同拍）、rdEn/rdData 读脉冲与读回数据、value 当前值（全宽）。
  */
class RegSwIO(totalW: Int) extends Bundle {
  val wrEn   = Output(Bool())
  val wrData = Output(UInt(totalW.W))
  val rdEn   = Output(Bool())
  val rdData = Output(UInt(totalW.W))
  val value  = Output(UInt(totalW.W))
}

/** RO 字段用户侧接口：硬件驱动只读值（软件读回） */
class RoHwIF(entries: Seq[(String, Int)]) extends Bundle {
  val value = new FieldInputRecord(entries)
}

/** W1C/RC 字段用户侧接口：硬件置位（电平或 1） */
class HwSetIF(entries: Seq[(String, Int)]) extends Bundle {
  val bits = new FieldInputRecord(entries)
}

/** W1S/RS 字段用户侧接口：硬件清除（电平与 0） */
class HwClrIF(entries: Seq[(String, Int)]) extends Bundle {
  val bits = new FieldInputRecord(entries)
}

/** W1T 字段用户侧接口：硬件翻转（电平异或） */
class HwTogIF(entries: Seq[(String, Int)]) extends Bundle {
  val bits = new FieldInputRecord(entries)
}

/** RW 字段用户侧接口：硬件直写（en 拉高时 data 覆盖字段，寄存器级使能） */
class HwWrIF(entries: Seq[(String, Int)]) extends Bundle {
  val en   = Input(Bool())
  val data = new FieldInputRecord(entries)
}

/**
 * 寄存器对用户侧的统一接口 Bundle（每个寄存器一个）。
 *
 * 按访问类型分组的子接口（组内为按字段名的输入端口，可直接 `:=` 赋值）：
 *  - sw    ：SW→HW 事件视图（wrEn/wrData/rdEn/rdData/value，Output）
 *  - ro    ：RO 字段驱动（.value）
 *  - hwSet ：W1C/RC 字段置位（.bits）
 *  - hwClr ：W1S/RS 字段清除（.bits）
 *  - hwTog ：W1T 字段翻转（.bits）
 *  - hwWr  ：RW 字段硬件直写（.en + .data）
 *
 * 无该类型字段的寄存器对应子接口为空 Record（0 元素），类型始终一致。
 */
class RegUserIO(alloc: RegAllocation) extends Bundle {
  private def entries(acc: AccessType*): Seq[(String, Int)] =
    alloc.fieldAllocations.collect {
      case fa if acc.contains(fa.field.access) => fa.field.name -> fa.field.bitWidth
    }

  val sw    = new RegSwIO(alloc.reg.expandedBits)
  val ro    = new RoHwIF(entries(AccessType.RO))
  val hwSet = new HwSetIF(entries(AccessType.W1C, AccessType.RC))
  val hwClr = new HwClrIF(entries(AccessType.W1S, AccessType.RS))
  val hwTog = new HwTogIF(entries(AccessType.W1T))
  val hwWr  = new HwWrIF(entries(AccessType.RW))
}

/** 按方向逐子接口连接"外层统一接口"与"内部寄存器 core"（正确处理 Input/Output） */
object CoreConnect {
  def apply(outer: RegUserIO, inner: RegUserIO): Unit = {
    outer.sw := inner.sw // 同向 Output，Bundle 级整体连接
    connectRecord(inner.ro.value,    outer.ro.value)
    connectRecord(inner.hwSet.bits,  outer.hwSet.bits)
    connectRecord(inner.hwClr.bits,  outer.hwClr.bits)
    connectRecord(inner.hwTog.bits,  outer.hwTog.bits)
    connectRecord(inner.hwWr.data,   outer.hwWr.data)
    inner.hwWr.en := outer.hwWr.en
  }
  /** 外层输入 → 内层输入（外层是源，内层是汇） */
  private def connectRecord(inner: Record, outer: Record): Unit = {
    outer.elements.foreach { case (n, o) =>
      inner.elements(n).asInstanceOf[UInt] := o.asInstanceOf[UInt]
    }
  }
}

/** 字段在寄存器内的位置（供硬件生成） */
case class FieldCfg(field: RegFieldDef, bitOffset: Int) {
  def name: String = field.name
  def width: Int = field.bitWidth
  def hi: Int = bitOffset + width - 1
}

/**
 * 单寄存器硬件模块（字段级语义，支持 >32bit 多字寄存器）。
 *
 * 多字（wordCount > 1，总线 32bit）两种模式：
 *  - 原子（atomic=true，默认）：写低字进入 shadow 暂存，写最高字时把
 *    {高字, shadow…} 合并一次提交（读任意字都返回已提交完整值的对应片段）；
 *  - 非原子（atomic=false）：逐字直接写入对应位域（中间态可被读观测）。
 *
 * 其他语义：
 *  - RO 字段由 io.core.ro.value 输入端口驱动，读回正常；
 *  - W1C/RC 支持 io.core.hwSet.bits 置位、W1S/RS 支持 io.core.hwClr.bits 清除、
 *    W1T 支持 io.core.hwTog.bits 翻转、RW 支持 io.core.hwWr.en/.data 直写；
 *  - io.core.sw.wrEn/wrData 为同一拍的写脉冲；io.core.sw.rdEn/rdData 为同一拍的读脉冲。
 *
 * 优先级（同拍冲突）：SW 写 > 读副作用(RC/RS) > HW set/clr/tog。
 */
class FieldReg(alloc: RegAllocation, dataWidth: Int) extends Module {
  private val totalW = alloc.reg.expandedBits
  private val wordCount = alloc.wordCount
  private val atomic = alloc.reg.atomic
  private val wordSelWidth = math.max(1, log2Ceil(wordCount))
  private val cfgs = alloc.fieldAllocations.map(fa => FieldCfg(fa.field, fa.bitOffset))
  require(cfgs.nonEmpty, s"reg '${alloc.reg.name}': no fields")
  require(cfgs.map(_.bitOffset).distinct.size == cfgs.size, s"reg '${alloc.reg.name}': overlapping fields")

  val io = IO(new Bundle {
    val dec  = new DecIO(dataWidth)
    val wordSel = Input(UInt(wordSelWidth.W))
    val core = new RegUserIO(alloc)
  })

  // 存储（RO 字段无存储）
  private val storages: Map[String, UInt] = cfgs.filter(_.field.access != AccessType.RO).map { cfg =>
    cfg.name -> RegInit(cfg.field.resetValue.U(cfg.width.W))
  }.toMap

  // ---------------- 读数据组装（无切片赋值） ----------------
  private val readVal = Wire(UInt(totalW.W))
  readVal := cfgs.foldLeft(0.U(totalW.W)) { case (acc, cfg) =>
    val v: UInt = cfg.field.access match {
      case AccessType.RO => io.core.ro.value.elements(cfg.name).asInstanceOf[UInt]
      case AccessType.WO => 0.U(cfg.width.W) // 只写字段读回 0
      case _             => storages(cfg.name)
    }
    val mask = ((BigInt(1) << cfg.width) - 1) << cfg.bitOffset
    (acc & ~mask.U(totalW.W)) | (v << cfg.bitOffset)
  }
  // 当前 word 的读回（word 间大端：wordSel=0（低地址）= 最高有效 word）
  // 位 word 索引 p = wordCount-1-wordSel（p=0 为最低位 word）
  private val bitWordIdx = (wordCount - 1).U - io.wordSel
  private val wordShift = bitWordIdx << log2Ceil(dataWidth)
  io.dec.rdata := ((readVal >> wordShift).pad(dataWidth))(dataWidth - 1, 0)

  // ---------------- 写（多字原子/非原子） ----------------
  /** 字段语义写：newBits 为字段全宽的新值 */
  def applyFieldSemantics(st: UInt, newBits: UInt, access: AccessType): Unit = {
    access match {
      case AccessType.RW | AccessType.WO | AccessType.RC | AccessType.RS => st := newBits
      case AccessType.W1C => st := st & ~newBits
      case AccessType.W1S => st := st | newBits
      case AccessType.W1T => st := st ^ newBits
      case AccessType.RO =>
    }
  }

  /** 把 v（宽度 hi-lo+1）贴到 st 的 [lo, hi] 位（组合） */
  def patchBits(st: UInt, lo: Int, hi: Int, v: UInt): UInt = {
    val w = st.getWidth
    val mask = ((BigInt(1) << (hi - lo + 1)) - 1) << lo
    (st & ~mask.U(w.W)) | ((v << lo).pad(w))(w - 1, 0)
  }

  /** 非原子：写 word w 时，只更新与该 word 相交的字段位 */
  def applyWordWrite(st: UInt, cfg: FieldCfg, w: Int): Unit = {
    val lo = cfg.bitOffset
    val hi = cfg.hi
    val wLo = w * dataWidth
    val wHi = w * dataWidth + dataWidth - 1
    val inLo = math.max(lo, wLo)
    val inHi = math.min(hi, wHi)
    if (inLo <= inHi) {
      val bits = io.dec.wdata(inHi - wLo, inLo - wLo) // 字段在 word 内的 bits
      val fLo = inLo - lo
      val fHi = inHi - lo
      cfg.field.access match {
        case AccessType.RW | AccessType.WO | AccessType.RC | AccessType.RS =>
          st := patchBits(st, fLo, fHi, bits)
        case AccessType.W1C =>
          st := st & ~(bits << fLo).pad(st.getWidth)
        case AccessType.W1S =>
          st := st | (bits << fLo)
        case AccessType.W1T =>
          st := st ^ (bits << fLo)
        case AccessType.RO =>
      }
    }
  }

  // ---------------- HW 写入路径（优先级最低：SW 写 > 读副作用 > HW） ----------------
  cfgs.foreach { cfg =>
    storages.get(cfg.name).foreach { st =>
      cfg.field.access match {
        case AccessType.W1C | AccessType.RC =>
          val hwSetF = io.core.hwSet.bits.elements(cfg.name).asInstanceOf[UInt]
          when(hwSetF.orR) { st := st | hwSetF }
        case AccessType.W1S | AccessType.RS =>
          val hwClrF = io.core.hwClr.bits.elements(cfg.name).asInstanceOf[UInt]
          when(hwClrF.orR) { st := st & ~hwClrF }
        case AccessType.W1T =>
          val hwTogF = io.core.hwTog.bits.elements(cfg.name).asInstanceOf[UInt]
          when(hwTogF.orR) { st := st ^ hwTogF }
        case AccessType.RW =>
          val hwWrF = io.core.hwWr.data.elements(cfg.name).asInstanceOf[UInt]
          when(io.core.hwWr.en) { st := hwWrF }
        case _ =>
      }
    }
  }

  // ---------------- 读副作用（RC/RS） ----------------
  when(io.dec.rd) {
    cfgs.foreach { cfg =>
      storages.get(cfg.name).foreach { st =>
        if (cfg.field.access == AccessType.RC) st := 0.U(cfg.width.W)
        if (cfg.field.access == AccessType.RS) st := ~0.U(cfg.width.W)
      }
    }
  }

  if (atomic) {
    // ---- 原子：非最高有效 word 进 shadow，写最高有效 word（wordSel=0，低地址）时提交 ----
    // word 间大端：wordSel=0 = 最高有效 word（低地址高有效）
    val lastW = 0 // 提交 word = wordSel 0（最高有效 word）
    val shadows = (0 until wordCount - 1).map { w =>
      val s = RegInit(0.U(dataWidth.W)); s.suggestName(s"shadow_w${w}"); s
    }
    // 提交值：位 word 索引 p = wordCount-1-wordSel；p==wordCount-1（最高有效 word）← wdata，
    // 其余 ← shadows(p)（shadows 按位 word 索引 p=0..wordCount-2 存）
    val commitVal = Wire(UInt(totalW.W))
    commitVal := (0 until wordCount).foldLeft(0.U(totalW.W)) { case (acc, p) =>
      val src = if (p == wordCount - 1) io.dec.wdata else shadows(p)
      val mask = (((BigInt(1) << dataWidth) - 1) << (p * dataWidth)) & ((BigInt(1) << totalW) - 1)
      (acc & ~mask.U(totalW.W)) | (src << (p * dataWidth))
    }
    when(io.dec.wr) {
      when(io.wordSel === 0.U) {
        // 写最高有效 word（低地址）→ 提交完整值
        cfgs.foreach { cfg =>
          storages.get(cfg.name).foreach { st =>
            applyFieldSemantics(st, commitVal(cfg.hi, cfg.bitOffset), cfg.field.access)
          }
        }
      }.otherwise {
        // 写其它 word → 进 shadow（位 word 索引 p = wordCount-1-wordSel）
        (0 until wordCount - 1).foreach { p =>
          when(bitWordIdx === p.U) { shadows(p) := io.dec.wdata }
        }
      }
    }
  } else {
    // ---- 非原子：逐 word 直接写（位 word 索引 p = wordCount-1-wordSel） ----
    when(io.dec.wr) {
      (0 until wordCount).foreach { p =>
        when(bitWordIdx === p.U) {
          cfgs.foreach { cfg =>
            storages.get(cfg.name).foreach { st => applyWordWrite(st, cfg, p) }
          }
        }
      }
    }
  }

  // ---------------- 用户侧输出 ----------------
  io.core.sw.wrEn   := RegNext(io.dec.wr, false.B)
  io.core.sw.wrData := (RegEnable(io.dec.wdata, io.dec.wr).pad(totalW))(totalW - 1, 0)
  io.core.sw.rdEn   := RegNext(io.dec.rd, false.B)
  io.core.sw.rdData := RegEnable(readVal, io.dec.rd)
  io.core.sw.value  := readVal
}

/** 按寄存器名的用户连接面（Record，每元素为统一接口 RegUserIO） */
class RegUserRecord(map: RegFileMap) extends Record {
  val elements: ListMap[String, RegUserIO] =
    ListMap(map.regs.map(a => a.reg.name -> new RegUserIO(a)): _*)
}

/** 存储器响应状态编码（用户侧 → RegFileTop） */
object MemStatus {
  /** 000：读数据 OK / 写操作完成 */
  val OK = "b000".U(3.W)
  /** 001：超时（读或写） */
  val TIMEOUT = "b001".U(3.W)
  /** 010：读数据不可纠正错误 */
  val UNCORRECTABLE = "b010".U(3.W)
  // 其余编码保留
}

/**
 * 存储器接口（对外 / 用户侧逻辑），请求-响应协议。
 *
 * 方向：RegFileTop 作为主人发起请求，用户侧逻辑（外部 SRAM 包装）响应。
 *  - 读请求：`rd` 拉高表示发起读（**在 ack 返回前一直保持高电平**），`raddr` 为请求地址；
 *  - 写请求：`wr` 拉高表示发起写（**在用户侧返回 ack 前一直保持高电平**），`waddr`/`wdata` 有效；
 *  - 响应：用户侧获得访问带宽/完成后，在返回数据或完成的那一拍**同拍**置起 `ack`，
 *    `rdata`（读）与 `status[2:0]` 同时有效；status 编码见 MemStatus：
 *    000 = 读数据 OK / 写完成，001 = 超时，010 = 读数据不可纠正错误，其余保留；
 *  - `rdata`/`ack`/`status` 为输入，未挂接时默认 0（ack=0 表示永远不响应）。
 *
 * 位宽可以大于总线位宽（32 的整数倍），由 RegFileTop 总线逻辑做原子/非原子拆分。
 */
class MemPortIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  // 请求（RegFileTop → 用户侧）：rd/wr 电平，ack 未返回前一直拉高
  val rd    = Output(Bool())     // 读请求（开始读数据）
  val raddr = Output(UInt(addrWidth.W))
  val wr    = Output(Bool())     // 写请求
  val waddr = Output(UInt(addrWidth.W))
  val wdata = Output(UInt(dataWidth.W))
  // 响应（用户侧 → RegFileTop）：ack 同拍返回 rdata（读）/ 完成（写）
  val rdata = Input(UInt(dataWidth.W))
  val ack   = Input(Bool())
  val status = Input(UInt(3.W))
}

/** 按存储器名的端口集合（每端口宽度随 MemoryDef 占据位宽） */
class MemPortRecord(mems: Seq[MemAllocation]) extends Record {
  val elements: ListMap[String, MemPortIO] =
    ListMap(mems.map(m => m.mem.name -> new MemPortIO(m.mem.addrWidth, m.mem.expandedDataWidth)): _*)
}

/** 同向连接两个 MemPortRecord（外层包装器 ↔ 内层 RegFileTop） */
object MemConnect {
  def apply(outer: MemPortRecord, inner: MemPortRecord): Unit = {
    outer.elements.foreach { case (n, o) =>
      val op = o.asInstanceOf[MemPortIO]
      val ip = inner.elements(n).asInstanceOf[MemPortIO]
      op.rd    := ip.rd
      op.raddr := ip.raddr
      op.wr    := ip.wr
      op.waddr := ip.waddr
      op.wdata := ip.wdata
      ip.rdata := op.rdata
      ip.ack   := op.ack
      ip.status := op.status
    }
  }
}

/**
 * 寄存器文件顶层：
 *  - 简单总线接口（wr/rd/addr/wdata/rdata，字节地址，单拍，dataWidth=32）；
 *  - io.user 为按寄存器名的用户连接面；
 *  - io.memPorts 为一片/多片 memory 地址空间的对外接口（外部 SRAM 挂接）；
 *    总线统一解码：寄存器地址 → 寄存器；memory 地址 → memory 接口（原子/非原子写）。
 */
class RegFileTop(map: RegFileMap, addrWidth: Int = 32, dataWidth: Int = 32) extends Module {
  require(dataWidth == 32, s"RegFileTop 目前仅支持 32bit 总线，got $dataWidth")

  val io = IO(new Bundle {
    val wr   = Input(Bool())
    val rd   = Input(Bool())
    val addr = Input(UInt(addrWidth.W))
    val wdata = Input(UInt(dataWidth.W))
    val rdata = Output(UInt(dataWidth.W))
    val user = new RegUserRecord(map)
    val memPorts = new MemPortRecord(map.mems)
  })

  // ---------------- 寄存器 ----------------
  private val regMods: Map[String, FieldReg] = map.regs.map { a =>
    val m = Module(new FieldReg(a, dataWidth))
    m.suggestName(s"reg_${a.reg.name}")
    a.reg.name -> m
  }.toMap

  private val regHits = map.regs.map { a =>
    val base = map.regBaseAddress + a.byteOffset
    if (a.wordCount <= 1) io.addr === base.U(addrWidth.W)
    else io.addr >= base.U(addrWidth.W) && io.addr < (base + a.byteSize).U(addrWidth.W)
  }
  private val regRdata = Wire(Vec(map.regs.size, UInt(dataWidth.W)))
  map.regs.zipWithIndex.foreach { case (a, i) =>
    val m = regMods(a.reg.name)
    m.io.dec.wr := io.wr && regHits(i)
    m.io.dec.rd := io.rd && regHits(i)
    m.io.dec.wdata := io.wdata
    val base = map.regBaseAddress + a.byteOffset
    if (a.wordCount <= 1) {
      m.io.wordSel := 0.U
    } else {
      val sel = (io.addr - base.U(addrWidth.W)) >> log2Ceil(dataWidth / 8)
      m.io.wordSel := sel(log2Ceil(a.wordCount) - 1, 0)
    }
    CoreConnect(io.user.elements(a.reg.name).asInstanceOf[RegUserIO], m.io.core)
    regRdata(i) := m.io.dec.rdata
  }

  // ---------------- Memory 地址空间（总线访问，原子/非原子） ----------------
  private val memHits: Seq[Bool] = map.mems.map { ma =>
    io.addr >= ma.baseAddress.U(addrWidth.W) && io.addr < (ma.baseAddress + ma.mem.byteSize).U(addrWidth.W)
  }
  private val memRdata: Seq[UInt] = map.mems.zip(memHits).map { case (ma, hit) =>
    val mem = ma.mem
    val eW = mem.expandedDataWidth          // 占据位宽（2 的幂，规则 3）
    val port = io.memPorts.elements(mem.name).asInstanceOf[MemPortIO]
    val offset = io.addr - ma.baseAddress.U(addrWidth.W)
    val unit = offset >> log2Ceil(eW / 8)   // expandedDataWidth 单元索引
    // word 间大端：wordInUnit（地址 word）=0 对应最高有效 word（位 word 索引 p = wordCount-1-wordInUnit）
    val wordInUnit: UInt =
      if (mem.wordCount == 1) 0.U(1.W)
      else (offset >> log2Ceil(dataWidth / 8))(log2Ceil(mem.wordCount) - 1, 0)
    val bitWordIdx = (mem.wordCount - 1).U - wordInUnit   // 位 word 索引（p=0 最低位 word）

    // 原子模式 shadow（低有效 word 暂存，写最高有效 word（wordInUnit=0）提交）
    val shadowMem =
      if (mem.atomic && mem.wordCount > 1)
        Some(Mem(mem.depth * mem.wordCount, UInt(dataWidth.W)))
      else None
    shadowMem.foreach(_.suggestName(s"shadow_${mem.name}"))

    // ================= 访问状态机（每 memory）=================
    // 状态：0=idle 1=读等待(rd 保持) 2=写等待(wr 保持) 3=RMW 读(rd 保持)
    // rd/wr 为电平信号：ack 未返回前一直拉高。
    val stIdle :: stRdWait :: stWrWait :: stRmwRead :: Nil = Enum(4)
    val memState = RegInit(stIdle)
    val memUnit  = RegInit(0.U(mem.addrWidth.W))
    val memWord  = RegInit(0.U(math.max(1, log2Ceil(mem.wordCount)).W))
    val memWdata = RegInit(0.U(eW.W))

    // 原子提交时的完整写数据（大端拼字：位 word p=wordCount-1 ← wdata（wordInUnit=0 写入），其余 ← shadow）
    def commitWdata: UInt = (0 until mem.wordCount).foldLeft(0.U(eW.W)) { case (acc, p) =>
      val src = if (p == mem.wordCount - 1) io.wdata
                else shadowMem.get.read((unit << log2Ceil(mem.wordCount)) | p.U)
      val mask = ((BigInt(1) << dataWidth) - 1) << (p * dataWidth)
      (acc & ~mask.U(eW.W)) | (src << (p * dataWidth))
    }

    switch(memState) {
      is(stIdle) {
        when(io.wr && hit) {
          if (mem.wordCount == 1) {
            // 单字：直接写请求（数据右对齐在占据空间低 bit，此处 expanded==dataWidth）
            memUnit := unit
            memWdata := io.wdata.pad(eW)
            memState := stWrWait
          } else if (mem.atomic) {
            when(wordInUnit === 0.U) {
              // 写最高有效 word（低地址）→ 提交完整值
              memUnit := unit
              memWdata := commitWdata
              memState := stWrWait
            }.otherwise {
              // 写低有效 word → 进 shadow（按位 word 索引 p = wordCount-1-wordInUnit）
              shadowMem.get.write((unit << log2Ceil(mem.wordCount)) | bitWordIdx, io.wdata)
            }
          } else {
            // 非原子多字：先读-改-写（读请求）
            memUnit := unit
            memWord := bitWordIdx
            memWdata := io.wdata
            memState := stRmwRead
          }
        }.elsewhen(io.rd && hit) {
          memUnit := unit
          memWord := bitWordIdx
          memState := stRdWait
        }
      }
      is(stRdWait) {
        when(port.ack) { memState := stIdle } // 读完成
      }
      is(stWrWait) {
        when(port.ack) { memState := stIdle } // 写完成（用户侧 status 上报）
      }
      is(stRmwRead) {
        when(port.ack) {
          memWdata := MuxLookup(memWord, port.rdata,
            (0 until mem.wordCount).map { p =>
              p.U -> patch(port.rdata, p * dataWidth, p * dataWidth + dataWidth - 1, memWdata, eW)
            })
          memState := stWrWait // 合并后写回
        }
      }
    }

    // 请求输出（电平：ack 未返回一直拉高）
    port.rd := (memState === stRdWait) || (memState === stRmwRead)
    port.raddr := memUnit
    port.wr := memState === stWrWait
    port.waddr := memUnit
    port.wdata := memWdata

    // 读响应：总线读请求的 ack 拍（state==stRdWait）采样；
    // status 非 MemStatus.OK 表示数据无效 → 总线读回 0
    val respData = Mux(port.status === MemStatus.OK, port.rdata, 0.U(eW.W))
    val respWord = (respData >> (memWord << log2Ceil(dataWidth)))(dataWidth - 1, 0)
    val respRegWord = RegEnable(respWord, port.ack && (memState === stRdWait))
    Mux(port.ack && (memState === stRdWait), respWord, respRegWord)
  }

  private def patch(st: UInt, lo: Int, hi: Int, v: UInt, width: Int): UInt = {
    val mask = ((BigInt(1) << (hi - lo + 1)) - 1) << lo
    (st & ~mask.U(width.W)) | ((v << lo).pad(width))(width - 1, 0)
  }

  io.rdata := MuxCase(0.U(dataWidth.W), regHits.zip(regRdata) ++ memHits.zip(memRdata))
}
