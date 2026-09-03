package FPP.Parser

import BaseCbb.data.{GenBundle, GenModule}
import chisel3._
import chisel3.util._

/**
 * Per-protocol parse functions and the parser FSM.
 *
 * Types (Bundles / constants / state encodings) live in [[ParserTypes]].
 *
 * ==字节序约定==
 * 报文第 k 个字节位于 `bits(8k+7, 8k)`，即 **首字节在低位**。
 * 每个 parse 函数都假设本层头部的第 0 字节位于入参 `bytes` 的 bits(7,0)，
 * 因此"跳过本层头部 N 字节"是 **逻辑右移** 8*N 位（见 `shiftBytes`）。
 *
 * ==流水线==
 * 每个解析级可选择插入一级寄存器（`ParserPipelineConfig`）。由于下游是
 * `Valid`（无反压），插入的级是 1 拍延迟线；FSM 用 `pipeIssued` 做
 * "已发射/未回收"握手，避免等待期间重复解析、重复记录 PHO/PHI。
 */

// ============= Parse Functions =============
// Each parse function returns: (fields, nextType, headerLen, newMeta, errorCode, headerType)

/**
 * Per-protocol parse functions.
 *
 * 所有函数都假设本层头部的第 0 字节位于入参 `bytes` 的最高位段，即：
 * 字节 k 位于 bits(511-8k, 504-8k)。于是
 *   - 字节 k          -> bytes(511 - 8*k, 504 - 8*k)
 *   - 从字节 k 起 w 字节的大端字段 -> bytes(511 - 8*k, 512 - 8*(k + w))
 * 大端字段（EtherType / total length / UDP 端口 ...）因此可以直接按数值读出，
 * 无需逐字段做字节反转。
 *
 * 每个函数返回 (fields, nextType, headerLen, newMeta, errorCode, headerType)。
 */
object parseEthernet {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // Ethernet II: DA(6) SA(6) EtherType(2)
    val etherType = bytes(415, 400)
    val nextType = MuxLookup(etherType, HeaderType.UNKNOWN)(
      Seq(
        EtherType.IPv4      -> HeaderType.IPV4,
        EtherType.IPv6      -> HeaderType.IPV6,
        EtherType.ARP       -> HeaderType.ARP,
        EtherType.VLAN      -> HeaderType.VLAN,
        EtherType.VLAN911   -> HeaderType.VLAN,
        EtherType.MPLS      -> HeaderType.MPLS,
        EtherType.MPLS_UNI  -> HeaderType.MPLS,
        EtherType.LLDP      -> HeaderType.UNKNOWN
      )
    )
    // VLAN 的 TPID 就占据 EtherType 字段。若其后还要解析 tag，必须把它留给
    // Vlan 状态，否则 Vlan 只会看到 TCI，永远判定 TPID 非法。
    val isTpid = etherType === EtherType.VLAN || etherType === EtherType.VLAN911
    val validEtherType = etherType === EtherType.IPv4 ||
                         etherType === EtherType.IPv6 ||
                         etherType === EtherType.ARP ||
                         etherType === EtherType.VLAN ||
                         etherType === EtherType.VLAN911 ||
                         etherType === EtherType.MPLS ||
                         etherType === EtherType.MPLS_UNI ||
                         etherType === EtherType.LLDP

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val errorCode = Mux(!validEtherType, HeaderErrorCode.InvalidEtherType,
                     Mux(etherType === 0.U, HeaderErrorCode.TruncatedHeader,
                       HeaderErrorCode.None))

    when(!validEtherType) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.InvalidEtherType
    }
    val nextTypeAdj = Mux(isTpid, HeaderType.VLAN, nextType)
    val hdrLen = Mux(isTpid, 12.U, 14.U)
    (0.U(512.W), nextTypeAdj, hdrLen, newMeta, errorCode, HeaderType.ETH)
  }
}

/** Extract VLAN tag fields (TPID + TCI = 4 bytes; the inner EtherType follows at byte 4) */
object parseVlan {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val tpid = bytes(511, 496)
    val tci = bytes(495, 480)
    // tag 之后的 2 字节才是内层 EtherType
    val innerType = bytes(479, 464)
    val nextType = MuxLookup(innerType, HeaderType.UNKNOWN)(
      Seq(
        EtherType.IPv4     -> HeaderType.IPV4,
        EtherType.IPv6     -> HeaderType.IPV6,
        EtherType.ARP      -> HeaderType.ARP,
        EtherType.VLAN     -> HeaderType.VLAN,
        EtherType.VLAN911  -> HeaderType.VLAN,
        EtherType.MPLS     -> HeaderType.MPLS,
        EtherType.MPLS_UNI -> HeaderType.MPLS
      )
    )

    val newMeta = Wire(new ParseMeta)
    newMeta := meta
    newMeta.vlanCount := meta.vlanCount + 1.U

    val validTpid = (tpid === EtherType.VLAN) || (tpid === EtherType.VLAN911)
    val errorCode = Mux(!validTpid, HeaderErrorCode.InvalidEtherType,
                     Mux(meta.vlanCount >= 7.U, HeaderErrorCode.VlanCountOverflow,
                       HeaderErrorCode.None))

    when(!validTpid) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.InvalidEtherType
    }
    when(meta.vlanCount >= 7.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.VlanCountOverflow
    }

    // tag 链未结束时只吃掉 TPID+TCI(4 字节)，让下一个 Vlan 状态仍能看到 TPID；
    // 链结束时连内层 EtherType 一起吃掉(6 字节)，下一层头部才是 IP/ARP。
    val hdrLen = Mux(nextType === HeaderType.VLAN, 4.U, 6.U)
    newMeta.errorInfo := errorCode

    (bytes(511, 480), nextType, hdrLen, newMeta, errorCode, HeaderType.VLAN)
  }
}

/** Extract MPLS label stack entry (4 bytes: Label(20) TC(3) S(1) TTL(8)) */
object parseMpls {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val label = bytes(511, 492)
    val tc    = bytes(491, 489)
    val bos   = bytes(488, 488).asBool
    val ttl   = bytes(487, 480)
    // S=1 表示栈底，其后通常是 IP；具体版本由 FSM 探测版本号决定
    val nextType = Mux(bos, HeaderType.IPV4, HeaderType.MPLS)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta
    newMeta.mplsCount := meta.mplsCount + 1.U

    val errorCode = Mux(meta.mplsCount >= 15.U, HeaderErrorCode.MplsCountOverflow,
                     HeaderErrorCode.None)

    when(meta.mplsCount >= 15.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.MplsCountOverflow
    }

    newMeta.errorInfo := errorCode

    (bytes(511, 480), nextType, 4.U, newMeta, errorCode, HeaderType.MPLS)
  }
}

/** Extract IPv4 header fields and validate checksum */
object parseIpv4 {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val version = bytes(511, 508)
    val headerLen = (bytes(507, 504) * 4.U)(5, 0)
    val totalLen = bytes(495, 480)
    val ttl = bytes(447, 440)
    val protocol = bytes(439, 432)

    // IPv4 首部校验和：所有 16 位字做反码和，正确的首部（含校验和字段）累加为 0xFFFF
    var sum = 0.U(16.W)
    for (i <- 0 until 10) {
      val word = bytes(511 - 16 * i, 496 - 16 * i)
      val sumWithCarry = sum +& word
      sum = sumWithCarry(15, 0) + sumWithCarry(16)
    }
    val checksumValid = sum === 0xFFFF.U(16.W)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta
    newMeta.checksumValid := checksumValid
    newMeta.totalLen := totalLen

    val errorCode = Mux(version =/= 4.U, HeaderErrorCode.Ipv4VersionError,
                     Mux(headerLen < 5.U, HeaderErrorCode.InvalidHeaderLength,
                     Mux(!checksumValid, HeaderErrorCode.Ipv4ChecksumError,
                     Mux(ttl === 0.U, HeaderErrorCode.Ipv4TtlZero,
                       HeaderErrorCode.None))))

    when(version =/= 4.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.Ipv4VersionError
    }
    when(headerLen < 5.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.InvalidHeaderLength
    }
    when(!checksumValid) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.Ipv4ChecksumError
    }
    when(ttl === 0.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.Ipv4TtlZero
    }

    val nextType = MuxLookup(protocol, HeaderType.UNKNOWN)(
      Seq(
        ProtocolType.TCP  -> HeaderType.TCP,
        ProtocolType.UDP  -> HeaderType.UDP,
        ProtocolType.ICMP -> HeaderType.ICMP,
        ProtocolType.GRE  -> HeaderType.GRE,
        ProtocolType.MPLS -> HeaderType.MPLS
      )
    )
    newMeta.errorInfo := errorCode

    (bytes(511, 352), nextType, headerLen, newMeta, errorCode, HeaderType.IPV4)
  }
}

/** Extract IPv6 header fields */
object parseIpv6 {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val version = bytes(511, 508)
    val payloadLen = bytes(479, 464)
    val nextHeader = bytes(463, 456)
    val hopLimit = bytes(455, 448)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta
    newMeta.totalLen := 40.U + payloadLen

    val errorCode = Mux(version =/= 6.U, HeaderErrorCode.Ipv4VersionError,
                     Mux(hopLimit === 0.U, HeaderErrorCode.Ipv6HopLimitZero,
                       HeaderErrorCode.None))

    when(version =/= 6.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.Ipv4VersionError
    }
    when(hopLimit === 0.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.Ipv6HopLimitZero
    }

    val nextType = MuxLookup(nextHeader, HeaderType.UNKNOWN)(
      Seq(
        ProtocolType.TCP     -> HeaderType.TCP,
        ProtocolType.UDP     -> HeaderType.UDP,
        ProtocolType.ICMPv6  -> HeaderType.ICMP,
        ProtocolType.GRE     -> HeaderType.GRE
      )
    )
    newMeta.errorInfo := errorCode

    (bytes(511, 192), nextType, 40.U, newMeta, errorCode, HeaderType.IPV6)
  }
}

/** Extract TCP header fields */
object parseTcp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // dataOffset 位于第 12 字节（高 4 位），单位为 4 字节
    val dataOffset = bytes(415, 408)
    val headerLen = (dataOffset(3, 0) * 4.U)(5, 0)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val errorCode = Mux(dataOffset < 5.U, HeaderErrorCode.TcpOffsetError,
                     HeaderErrorCode.None)

    when(dataOffset < 5.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.TcpOffsetError
    }

    newMeta.errorInfo := errorCode

    (bytes(511, 352), HeaderType.PAYLOAD, headerLen, newMeta, errorCode, HeaderType.TCP)
  }
}

/** Extract UDP header fields and determine tunnel type */
object parseUdp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val dstPort = bytes(495, 480)
    val length = bytes(479, 464)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val errorCode = Mux(length < 8.U, HeaderErrorCode.UdpLengthError,
                     HeaderErrorCode.None)

    when(length < 8.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.UdpLengthError
    }

    val nextType = MuxLookup(dstPort, HeaderType.PAYLOAD)(
      Seq(
        4789.U -> HeaderType.VXLAN,
        6081.U -> HeaderType.GENEVE,
        2152.U -> HeaderType.GTPU,
        2123.U -> HeaderType.GTPU
      )
    )
    newMeta.errorInfo := errorCode

    (bytes(511, 448), nextType, 8.U, newMeta, errorCode, HeaderType.UDP)
  }
}

/** Extract ICMP header fields */
object parseIcmp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    (bytes(511, 448), HeaderType.PAYLOAD, 8.U, meta, HeaderErrorCode.None, HeaderType.ICMP)
  }
}

/** Extract GRE header fields (RFC 2784: C/K/S flags, version, protocol type) */
object parseGre {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // 32 位首字: C(bit0) R K S ... Flags | Ver(bit13..15)，其后 16 位为 Protocol Type
    val hasChecksum = bytes(511, 511).asBool // C
    val hasKey = bytes(509, 509).asBool      // K
    val hasSequence = bytes(508, 508).asBool // S
    val version = bytes(498, 496)
    val protoType = bytes(495, 480)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val errorCode = Mux(version =/= 0.U, HeaderErrorCode.GreVersionError,
                     HeaderErrorCode.None)

    when(version =/= 0.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.GreVersionError
    }

    val headerLen = 4.U + Mux(hasChecksum, 4.U, 0.U) +
                        Mux(hasKey, 4.U, 0.U) +
                        Mux(hasSequence, 4.U, 0.U)

    val nextType = MuxLookup(protoType, HeaderType.PAYLOAD)(
      Seq(
        EtherType.IPv4 -> HeaderType.IPV4,
        EtherType.IPv6 -> HeaderType.IPV6
      )
    )
    newMeta.errorInfo := errorCode

    (bytes(511, 480), nextType, headerLen, newMeta, errorCode, HeaderType.GRE)
  }
}

/** Extract VXLAN header fields (8 bytes; the inner frame is Ethernet) */
object parseVxlan {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    (bytes(511, 448), HeaderType.ETH, 8.U, meta, HeaderErrorCode.None, HeaderType.VXLAN)
  }
}

/** Extract Geneve header fields (8 bytes + options; the inner frame is Ethernet) */
object parseGeneve {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // byte0: Ver(7:6) OptLen(5:0)，单位为 4 字节
    val optLen = bytes(509, 504)
    val headerLen = 8.U + (optLen * 4.U)(7, 0)
    (bytes(511, 448), HeaderType.ETH, headerLen, meta, HeaderErrorCode.None, HeaderType.GENEVE)
  }
}

/** Extract GTPU header fields (8 bytes + optional extension / sequence / PDU session) */
object parseGtpu {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // byte0: Ver(7:5) PT(4) spare(3) E(2) S(1) PN(0)
    val hasExtension = bytes(506, 506).asBool // E
    val hasSequence = bytes(505, 505).asBool  // S
    val hasPduSession = bytes(504, 504).asBool // PN

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val headerLen = 8.U + Mux(hasExtension, 4.U, 0.U) +
                        Mux(hasSequence, 4.U, 0.U) +
                        Mux(hasPduSession, 4.U, 0.U)
    (bytes(511, 448), HeaderType.IPV4, headerLen, newMeta, HeaderErrorCode.None, HeaderType.GTPU)
  }
}

/** Extract NSH header fields */
object parseNsh {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // byte0: Ver(7:6) O(5) C(4) Len[5:4](3:0)；byte1: Len[3:0](7:4) ...
    val length = Cat(bytes(507, 504), bytes(503, 502))
    val nextProto = bytes(487, 480)
    val headerLen = (length * 4.U)(7, 0)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val nextType = MuxLookup(nextProto, HeaderType.UNKNOWN)(
      Seq(
        1.U -> HeaderType.NSH,
        2.U -> HeaderType.IPV4,
        3.U -> HeaderType.IPV6,
        4.U -> HeaderType.IPV4
      )
    )
    (bytes(511, 448), nextType, headerLen, newMeta, HeaderErrorCode.None, HeaderType.NSH)
  }
}

/** Extract ARP header fields */
object parseArp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    (bytes(511, 288), HeaderType.PAYLOAD, 28.U, meta, HeaderErrorCode.None, HeaderType.ARP)
  }
}

// ============= Pipeline Stage Wrapper =============

/**
 * Single-stage registered pipeline.
 *
 * 下游是 `Valid`（没有反压），所以这里的出口按"每拍必被取走"处理：
 * 本质是一条 1 拍延迟线（in.ready 恒 1，out.valid = 上一拍的 in.valid）。
 * 时序上的收益来自把组合逻辑切断，而不是靠反压来攒数据。
 */
class PipelineStage[T <: Data](gen: T) extends GenModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen))
    val out = DecoupledIO(gen)
  })

  val validReg = RegInit(false.B)
  val bitsReg = Reg(chiselTypeOf(io.in.bits))

  io.in.ready := true.B
  when(io.in.valid) {
    validReg := true.B
    bitsReg := io.in.bits
  }.otherwise {
    validReg := false.B
  }

  io.out.valid := validReg
  io.out.bits := bitsReg
}

object PipelineStage {
  def apply[T <: Data](in: DecoupledIO[T], enable: Boolean): DecoupledIO[T] = {
    if (enable) {
      val pipe = Module(new PipelineStage(in.bits.cloneType))
      pipe.io.in <> in
      pipe.io.out.ready := true.B
      pipe.io.out
    } else {
      in.ready := true.B
      in
    }
  }
}

// ============= Main Parser Core =============
class ParserCore(
  val pipelineConfig: ParserPipelineConfig = ParserPipelineConfig()
) extends GenModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(UInt(512.W)))
    val out = ValidIO(new ParseResult)
    val parseDone = Output(Bool())
    val meta = Output(new ParseMeta)
  })

  // Parser state machine
  val state = RegInit(ParserState.Idle)
  val nextState = Wire(chiselTypeOf(state))

  // Working registers
  val workBytes = Reg(UInt(512.W))
  val workMeta = Reg(new ParseMeta)
  val workNextType = Reg(UInt(8.W))
  val workParsedBytes = Reg(UInt(16.W))

  // Header tracking registers (PHO/PHI)
  val headerOffsets = Reg(Vec(24, UInt(16.W)))
  val headerDescs = Reg(Vec(24, new PacketHeaderDesc))
  val headerCount = Reg(UInt(5.W))

  // 流水线握手：本级已发射但结果尚未回收。等待期间不得重复解析 / 重复记录 PHO。
  val pipeIssued = RegInit(false.B)

  // Output bundle (combinational -- the registers below are updated on the same
  // cycle as io.out.valid, so a registered result would lag one packet behind).
  val outBits = Wire(new ParseResult)
  outBits := 0.U.asTypeOf(new ParseResult)

  // Default outputs
  io.out.valid := false.B
  io.out.bits := outBits
  io.parseDone := false.B
  io.meta := workMeta

  /** 跳过刚解析完的 `by` 字节（字节 0 在最高位段，故左移把下一层头部顶到最高位）。 */
  def shiftBytes(bytes: UInt, by: UInt): UInt = bytes << (by * 8.U)

  // Helper: convert Valid to Decoupled for pipeline stage
  def validToDecoupled[T <: Data](in: ValidIO[T]): DecoupledIO[T] = {
    val d = Wire(DecoupledIO(chiselTypeOf(in.bits)))
    d.valid := in.valid
    d.bits := in.bits
    d.ready := false.B
    d
  }

  // Helper: create pipeline stage if enabled
  def pipeAfter[T <: Data](data: ValidIO[T], enable: Boolean): ValidIO[T] = {
    val dec = validToDecoupled(data)
    val piped = PipelineStage(dec, enable)
    val result = Wire(Valid(chiselTypeOf(data.bits)))
    result.valid := piped.valid
    result.bits := piped.bits
    result
  }

  // Helper: record header in PHO/PHI arrays
  def recordHeader(offset: UInt, hdrType: UInt, length: UInt, valid: Bool, errorCode: UInt) = {
    when(headerCount < 24.U) {
      headerOffsets(headerCount) := offset
      headerDescs(headerCount).headerType := hdrType
      headerDescs(headerCount).offset := offset
      headerDescs(headerCount).length := length
      headerDescs(headerCount).valid := valid
      headerDescs(headerCount).errorCode := errorCode
      headerCount := headerCount + 1.U
    }
  }

  /**
   * 发射一级解析结果进入（可选的）级间流水线，并在结果回收时提交到工作寄存器。
   *
   * @return (out, advance) —— `advance` 为真的那一拍 FSM 才能前进
   */
  def issueStage(
      bytesNext: UInt,
      metaNext: ParseMeta,
      nextTypeNext: UInt,
      parsedNext: UInt,
      ok: Bool,
      pipeEnable: Boolean
  ): (ValidIO[InterstageData], Bool) = {
    val in = Wire(Valid(new InterstageData))
    in.valid := !pipeIssued && ok
    in.bits.bytes := bytesNext
    in.bits.meta := metaNext
    in.bits.nextType := nextTypeNext
    in.bits.parsedBytes := parsedNext
    in.bits.valid := ok

    val out = pipeAfter(in, pipeEnable)
    // 解析出错时结果不会进入流水线，但错误标志必须落到 workMeta 上，
    // 否则 io.meta / out.bits.valid 反映不出失败原因。
    when(!ok) {
      workMeta := metaNext
    }
    when(out.valid) {
      workBytes := out.bits.bytes
      workMeta := out.bits.meta
      workNextType := out.bits.nextType
      workParsedBytes := out.bits.parsedBytes
      pipeIssued := false.B
    }.elsewhen(in.valid) {
      pipeIssued := true.B
    }
    (out, out.valid)
  }

  /** Fill the result bundle from the current working set. */
  def emitResult(valid: Bool): Unit = {
    outBits.fields := workBytes
    outBits.nextType := workNextType
    outBits.headerLen := workParsedBytes
    outBits.valid := valid
    outBits.meta := workMeta
    outBits.headerCount := headerCount
    for (i <- 0 until 24) {
      outBits.pho(i) := headerOffsets(i)
      outBits.phi(i) := headerDescs(i)
    }
    io.out.valid := true.B
    io.parseDone := true.B
  }

  // Compute next state based on current state
  nextState := state
  switch(state) {
    is(ParserState.Idle) {
      when(io.in.valid) {
        workBytes := io.in.bits
        workMeta := 0.U.asTypeOf(new ParseMeta)
        workMeta.totalLen := 512.U
        workMeta.checksumValid := true.B
        workParsedBytes := 0.U
        workNextType := HeaderType.UNKNOWN
        headerCount := 0.U
        pipeIssued := false.B
        nextState := ParserState.Eth
      }
    }

    is(ParserState.Eth) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseEthernet(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterEth
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        // workBytes 的更新要到下一拍才生效，这里读到的仍是本层头部
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.IPV4 -> ParserState.Ipv4,
            HeaderType.IPV6 -> ParserState.Ipv6,
            HeaderType.ARP  -> ParserState.Arp,
            HeaderType.VLAN -> ParserState.Vlan,
            HeaderType.MPLS -> ParserState.Mpls
          )
        )
      }
    }

    is(ParserState.Vlan) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseVlan(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterVlan
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.IPV4 -> ParserState.Ipv4,
            HeaderType.IPV6 -> ParserState.Ipv6,
            HeaderType.ARP  -> ParserState.Arp,
            HeaderType.VLAN -> ParserState.Vlan,
            HeaderType.MPLS -> ParserState.Mpls
          )
        )
      }
    }

    is(ParserState.Mpls) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseMpls(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterMpls
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        // workBytes 要到下一拍才更新，下一层头部的首字节在 out.bits.bytes 里
        when(out.bits.nextType === HeaderType.MPLS) {
          nextState := ParserState.Mpls
        }.otherwise {
          // 栈底之后的猜测：用 IP 版本号区分 v4 / v6，否则当纯载荷
          val ver = out.bits.bytes(511, 508)
          nextState := Mux(ver === 4.U, ParserState.Ipv4,
                       Mux(ver === 6.U, ParserState.Ipv6,
                         ParserState.Payload))
        }
      }
    }

    is(ParserState.Ipv4) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIpv4(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterIpv4
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.TCP  -> ParserState.Tcp,
            HeaderType.UDP  -> ParserState.Udp,
            HeaderType.ICMP -> ParserState.Icmp,
            HeaderType.GRE  -> ParserState.TunnelGre,
            HeaderType.MPLS -> ParserState.Mpls
          )
        )
      }
    }

    is(ParserState.Ipv6) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIpv6(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterIpv6
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.TCP  -> ParserState.Tcp,
            HeaderType.UDP  -> ParserState.Udp,
            HeaderType.ICMP -> ParserState.Icmp,
            HeaderType.GRE  -> ParserState.TunnelGre
          )
        )
      }
    }

    is(ParserState.Arp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseArp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterArp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Done
      }
    }

    is(ParserState.Tcp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseTcp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterTcp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Done
      }
    }

    is(ParserState.Udp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseUdp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterUdp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Done)(
          Seq(
            HeaderType.VXLAN  -> ParserState.TunnelVxlan,
            HeaderType.GENEVE -> ParserState.TunnelGeneve,
            HeaderType.GTPU   -> ParserState.TunnelGtpu
          )
        )
      }
    }

    is(ParserState.Icmp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIcmp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterIcmp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Done
      }
    }

    is(ParserState.TunnelVxlan) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseVxlan(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterVxlan
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Eth   // VXLAN 内层是完整以太网帧
      }
    }

    is(ParserState.TunnelGeneve) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGeneve(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterGeneve
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Eth   // Geneve 内层是完整以太网帧
      }
    }

    is(ParserState.TunnelGtpu) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGtpu(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterGtpu
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        // GTPU 内层是 IP，用版本号区分 v4 / v6（下一层头部在 out.bits.bytes）
        val ver = out.bits.bytes(511, 508)
        nextState := Mux(ver === 4.U, ParserState.Ipv4,
                     Mux(ver === 6.U, ParserState.Ipv6,
                       ParserState.Done))
      }
    }

    is(ParserState.TunnelGre) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGre(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterGre
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Done)(
          Seq(
            HeaderType.IPV4 -> ParserState.Ipv4,
            HeaderType.IPV6 -> ParserState.Ipv6
          )
        )
      }
    }

    is(ParserState.Payload) {
      recordHeader(workParsedBytes, HeaderType.PAYLOAD, 0.U, true.B, HeaderErrorCode.None)
      nextState := ParserState.Done
    }

    is(ParserState.Done) {
      emitResult(!workMeta.parseError)
      headerCount := 0.U
      pipeIssued := false.B
      nextState := ParserState.Idle
    }

    is(ParserState.Error) {
      emitResult(false.B)
      headerCount := 0.U
      pipeIssued := false.B
      nextState := ParserState.Idle
    }
  }

  // Update state
  state := nextState

  // Input ready signal
  io.in.ready := (state === ParserState.Idle) ||
                 (state === ParserState.Done) ||
                 (state === ParserState.Error)
}

// ============= Companion Object =============

object ParserCore {
  def apply(): ParserCore = Module(new ParserCore(ParserPipelineConfig.default))
}
