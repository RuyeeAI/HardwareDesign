package FPP.Parser

import BaseCbb.GenModule
import BaseCbb.GenBundle
import chisel3._
import chisel3.util._

// ============= Constants =============
object HeaderType {
  val NONE    = 0.U(8.W)
  val ETH     = 1.U(8.W)
  val VLAN    = 2.U(8.W)
  val MPLS    = 3.U(8.W)
  val IPV4    = 4.U(8.W)
  val IPV6    = 5.U(8.W)
  val TCP     = 6.U(8.W)
  val UDP     = 7.U(8.W)
  val ICMP    = 8.U(8.W)
  val ARP     = 9.U(8.W)
  val GRE     = 10.U(8.W)
  val VXLAN   = 11.U(8.W)
  val GENEVE  = 12.U(8.W)
  val GTPU    = 13.U(8.W)
  val NSH     = 14.U(8.W)
  val PAYLOAD = 15.U(8.W)
  val UNKNOWN = 16.U(8.W)
}

// ============= Error Codes =============
object HeaderErrorCode extends ChiselEnum {
  val None                = 0.U(4.W)
  val InvalidEtherType    = 1.U(4.W)
  val Ipv4ChecksumError   = 2.U(4.W)
  val InvalidProtocol     = 3.U(4.W)
  val TruncatedHeader     = 4.U(4.W)
  val InvalidHeaderLength = 5.U(4.W)
  val VlanCountOverflow   = 6.U(4.W)
  val MplsCountOverflow   = 7.U(4.W)
  val TunnelNotSupported   = 8.U(4.W)
  val PayloadTooShort     = 9.U(4.W)
  val Ipv4TtlZero         = 10.U(4.W)
  val Ipv4VersionError    = 11.U(4.W)
  val Ipv6HopLimitZero    = 12.U(4.W)
  val TcpOffsetError      = 13.U(4.W)
  val UdpLengthError      = 14.U(4.W)
  val GreVersionError     = 15.U(4.W)
}

// ============= Packet Header Descriptor =============
class PacketHeaderDesc extends GenBundle {
  val headerType = UInt(8.W)    // Protocol type (HeaderType.xxx)
  val offset = UInt(16.W)        // Byte offset from packet start
  val length = UInt(8.W)        // Header length in bytes
  val valid = Bool()            // Header parsed successfully
  val errorCode = UInt(4.W)      // Error code if valid=false
}

// ============= Parse Metadata =============
class ParseMeta extends GenBundle {
  val totalLen = UInt(16.W)       // Total packet length
  val parsedLen = UInt(16.W)       // Bytes parsed so far
  val vlanCount = UInt(3.W)       // Number of VLAN tags parsed
  val mplsCount = UInt(4.W)       // Number of MPLS labels
  val checksumValid = Bool()       // Checksum validation result
  val parseError = Bool()          // Any parse error
  val errorInfo = UInt(4.W)       // Error code for debugging
}

// ============= Parse Result =============
class ParseResult extends GenBundle {
  val fields = UInt(512.W)         // Extracted fields
  val nextType = UInt(8.W)         // Next protocol type
  val headerLen = UInt(8.W)        // Current header length in bytes
  val valid = Bool()               // Parsing valid
  val meta = new ParseMeta         // Metadata pass-through

  // Packet Header Offset array (max 24 headers)
  val pho = Vec(24, UInt(16.W))
  // Packet Header Information array
  val phi = Vec(24, new PacketHeaderDesc)
  // Number of headers parsed
  val headerCount = UInt(5.W)
}

// ============= Parser States =============
object ParserState extends ChiselEnum {
  val Idle = 0.U(8.W)
  val Eth = 1.U(8.W)
  val Vlan = 2.U(8.W)
  val QinQ = 3.U(8.W)
  val Mpls = 4.U(8.W)
  val Ipv4 = 5.U(8.W)
  val Ipv6 = 6.U(8.W)
  val Arp = 7.U(8.W)
  val Tcp = 8.U(8.W)
  val Udp = 9.U(8.W)
  val Icmp = 10.U(8.W)
  val TunnelVxlan = 11.U(8.W)
  val TunnelGeneve = 12.U(8.W)
  val TunnelGtpu = 13.U(8.W)
  val TunnelGre = 14.U(8.W)
  val TunnelNsh = 15.U(8.W)
  val Payload = 16.U(8.W)
  val Done = 17.U(8.W)
  val Error = 18.U(8.W)
}

// ============= Parse Functions =============
// Each parse function returns: (fields, nextType, headerLen, newMeta, errorCode, headerType)

/** Extract Ethernet header fields and determine next protocol type */
object parseEthernet {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val etherType = bytes(15, 0)
    val nextType = MuxLookup(etherType, ProtocolType.UNKNOWN)(
      Seq(
        EtherType.IPv4 -> ProtocolType.TCP,
        EtherType.IPv6 -> ProtocolType.TCP,
        EtherType.ARP  -> ProtocolType.UNKNOWN,
        EtherType.VLAN -> EtherType.VLAN,
        EtherType.VLAN911 -> EtherType.VLAN911,
        EtherType.MPLS -> ProtocolType.MPLS,
        EtherType.MPLS_UNI -> ProtocolType.MPLS,
        EtherType.LLDP -> ProtocolType.UNKNOWN
      )
    )
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
    (0.U(512.W), nextType, 14.U, newMeta, errorCode, HeaderType.ETH)
  }
}

/** Extract VLAN tag fields */
object parseVlan {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // After shiftBytes, header starts at bit 0, so TPID is at bits 15:0, inner type at 31:16
    val tpid = bytes(15, 0)
    val innerType = bytes(31, 16)
    val nextType = MuxLookup(innerType, ProtocolType.UNKNOWN)(
      Seq(
        EtherType.IPv4 -> ProtocolType.TCP,
        EtherType.IPv6 -> ProtocolType.TCP,
        EtherType.ARP  -> ProtocolType.UNKNOWN,
        EtherType.VLAN -> EtherType.VLAN,
        EtherType.VLAN911 -> EtherType.VLAN911,
        EtherType.MPLS -> ProtocolType.MPLS,
        EtherType.MPLS_UNI -> EtherType.MPLS
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

    // Keep only the bytes after VLAN header for next stage
    (bytes(31, 0), nextType, 4.U, newMeta, errorCode, HeaderType.VLAN)
  }
}

/** Extract MPLS label stack entry */
object parseMpls {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val bos = bytes(8, 8).asBool
    val label = bytes(31, 12)
    val nextType = Mux(bos, ProtocolType.TCP, ProtocolType.MPLS)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta
    newMeta.mplsCount := meta.mplsCount + 1.U

    val errorCode = Mux(meta.mplsCount >= 15.U, HeaderErrorCode.MplsCountOverflow,
                     HeaderErrorCode.None)

    when(meta.mplsCount >= 15.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.MplsCountOverflow
    }

    (bytes(31, 0), nextType, 4.U, newMeta, errorCode, HeaderType.MPLS)
  }
}

/** Extract IPv4 header fields and validate checksum */
object parseIpv4 {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val version = bytes(3, 0)
    val headerLen = (bytes(7, 4) * 4.U)(5, 0)
    val totalLen = bytes(31, 16)
    val ttl = bytes(71, 64)
    val protocol = bytes(79, 72)

    // Calculate IPv4 checksum (sum of all 16-bit words, one's complement)
    var sum = 0.U(16.W)
    for (i <- 0 until 10) {
      val word = bytes(16 * (i + 1) - 1, 16 * i)
      val sumWithCarry = sum +& word
      sum = sumWithCarry(15, 0) + sumWithCarry(16)
    }
    val checksum = ~sum(15, 0)
    val checksumValid = checksum === 0.U

    val newMeta = Wire(new ParseMeta)
    newMeta := meta
    newMeta.checksumValid := checksumValid

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

    val nextType = MuxLookup(protocol, ProtocolType.UNKNOWN)(
      Seq(
        ProtocolType.TCP  -> ProtocolType.TCP,
        ProtocolType.UDP  -> ProtocolType.UDP,
        ProtocolType.ICMP -> ProtocolType.ICMP,
        ProtocolType.GRE  -> ProtocolType.GRE,
        ProtocolType.MPLS -> ProtocolType.MPLS
      )
    )
    (bytes(159, 0), nextType, headerLen, newMeta, errorCode, HeaderType.IPV4)
  }
}

/** Extract IPv6 header fields */
object parseIpv6 {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val version = bytes(3, 0)
    val hopLimit = bytes(63, 56)
    val nextHeader = bytes(55, 48)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

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

    val nextType = MuxLookup(nextHeader, ProtocolType.UNKNOWN)(
      Seq(
        ProtocolType.TCP     -> ProtocolType.TCP,
        ProtocolType.UDP     -> ProtocolType.UDP,
        ProtocolType.ICMPv6  -> ProtocolType.ICMP,
        ProtocolType.GRE     -> ProtocolType.GRE
      )
    )
    (bytes(319, 0), nextType, 40.U, newMeta, errorCode, HeaderType.IPV6)
  }
}

/** Extract TCP header fields */
object parseTcp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // TCP header layout after shiftBytes (header starts at bit 0):
    // srcPort(15:0), dstPort(31:16), seqNum(63:32), ackNum(95:64),
    // dataOffset+flags(111:96), window(127:112), checksum(143:128), urgentPtr(159:144)
    val dataOffset = bytes(111, 104)
    val headerLen = (Cat(0.U(4.W), dataOffset(3, 0)) * 4.U)(5, 0)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val errorCode = Mux(dataOffset < 5.U, HeaderErrorCode.TcpOffsetError,
                     HeaderErrorCode.None)

    when(dataOffset < 5.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.TcpOffsetError
    }

    (bytes(159, 0), ProtocolType.UNKNOWN, headerLen, newMeta, errorCode, HeaderType.TCP)
  }
}

/** Extract UDP header fields and determine tunnel type */
object parseUdp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // UDP header layout: srcPort(15:0), dstPort(31:16), length(47:32), checksum(63:48)
    val srcPort = bytes(15, 0)
    val dstPort = bytes(31, 16)
    val length = bytes(47, 32)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val errorCode = Mux(length < 8.U, HeaderErrorCode.UdpLengthError,
                     HeaderErrorCode.None)

    when(length < 8.U) {
      newMeta.parseError := true.B
      newMeta.errorInfo := HeaderErrorCode.UdpLengthError
    }

    val nextType = MuxLookup(dstPort, ProtocolType.UNKNOWN)(
      Seq(
        4789.U -> ProtocolType.VXLAN,
        6081.U -> ProtocolType.GENEVE,
        2152.U -> ProtocolType.GTPU,
        2123.U -> ProtocolType.GTPU
      )
    )
    (bytes(63, 0), nextType, 8.U, newMeta, errorCode, HeaderType.UDP)
  }
}

/** Extract ICMP header fields */
object parseIcmp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    (bytes(63, 0), ProtocolType.UNKNOWN, 8.U, meta, HeaderErrorCode.None, HeaderType.ICMP)
  }
}

/** Extract GRE header fields */
object parseGre {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    // GRE header format (after shiftBytes, header starts at bit 0):
    // Bits 0-15: C(1), R(1), K(1), S(1), Flags(4), Version(3), Protocol Type(16)
    // After shiftBytes: bytes(0) = C, bytes(5) = K, bytes(6) = S, bytes(13,11) = version, bytes(31,16) = protoType
    val hasChecksum = bytes(0, 0).asBool
    val hasKey = bytes(5, 5).asBool
    val hasSequence = bytes(6, 6).asBool
    val version = bytes(13, 11)
    val protoType = bytes(31, 16)

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

    val nextType = MuxLookup(protoType, ProtocolType.UNKNOWN)(
      Seq(
        EtherType.IPv4 -> EtherType.IPv4,
        EtherType.IPv6 -> EtherType.IPv6
      )
    )
    (bytes(31, 0), nextType, headerLen, newMeta, errorCode, HeaderType.GRE)
  }
}

/** Extract VXLAN header fields */
object parseVxlan {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    (bytes(63, 0), EtherType.IPv4, 8.U, meta, HeaderErrorCode.None, HeaderType.VXLAN)
  }
}

/** Extract Geneve header fields */
object parseGeneve {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val optLen = bytes(7, 2)
    val headerLen = 8.U + (Cat(0.U(2.W), optLen) * 4.U)(5, 0)
    (bytes(63, 0), EtherType.IPv4, headerLen, meta, HeaderErrorCode.None, HeaderType.GENEVE)
  }
}

/** Extract GTPU header fields */
object parseGtpu {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val hasExtension = bytes(2, 2).asBool
    val hasSequence = bytes(3, 3).asBool
    val hasPduSession = bytes(4, 4).asBool

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val headerLen = 8.U + Mux(hasExtension, 4.U, 0.U) +
                        Mux(hasSequence, 4.U, 0.U) +
                        Mux(hasPduSession, 4.U, 0.U)
    (bytes(63, 0), EtherType.IPv4, headerLen, newMeta, HeaderErrorCode.None, HeaderType.GTPU)
  }
}

/** Extract NSH header fields */
object parseNsh {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    val length = bytes(15, 10)
    val nextProto = bytes(31, 24)
    val headerLen = (Cat(0.U(2.W), length) * 4.U)(5, 0)

    val newMeta = Wire(new ParseMeta)
    newMeta := meta

    val nextType = MuxLookup(nextProto, ProtocolType.UNKNOWN)(
      Seq(
        1.U -> ProtocolType.NSH,
        2.U -> EtherType.IPv4,
        3.U -> EtherType.IPv6,
        4.U -> EtherType.IPv4
      )
    )
    (bytes(71, 0), nextType, headerLen, newMeta, HeaderErrorCode.None, HeaderType.NSH)
  }
}

/** Extract ARP header fields */
object parseArp {
  def apply(bytes: UInt, meta: ParseMeta): (UInt, UInt, UInt, ParseMeta, UInt, UInt) = {
    (bytes(223, 0), ProtocolType.UNKNOWN, 28.U, meta, HeaderErrorCode.None, HeaderType.ARP)
  }
}

// ============= Pipeline Stage Wrapper =============

/** Single-stage registered pipeline for a specific data type */
class PipelineStage[T <: Data](gen: T) extends GenModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(gen))
    val out = DecoupledIO(gen)
  })

  val validReg = RegInit(false.B)
  val bitsReg = Reg(chiselTypeOf(io.in.bits))

  io.in.ready := !validReg || io.out.ready

  when(io.in.valid && !validReg) {
    validReg := true.B
    bitsReg := io.in.bits
  }.elsewhen(io.out.ready && validReg) {
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
      pipe.io.out
    } else {
      in
    }
  }
}

// ============= Interstage Data Bundle =============

/** Data passed between parser stages */
class InterstageData extends GenBundle {
  val bytes = UInt(512.W)
  val meta = new ParseMeta
  val nextType = UInt(8.W)
  val parsedBytes = UInt(16.W)
  val valid = Bool()
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
  val nextState = Wire(ParserState())

  // Working registers
  val workBytes = Reg(UInt(512.W))
  val workMeta = Reg(new ParseMeta)
  val workNextType = Reg(UInt(8.W))
  val workParsedBytes = Reg(UInt(16.W))

  // NEW: Header tracking registers
  val headerOffsets = Reg(Vec(24, UInt(16.W)))
  val headerDescs = Reg(Vec(24, new PacketHeaderDesc))
  val headerCount = Reg(UInt(5.W))
  val headerTypes = Reg(Vec(24, UInt(8.W)))  // Track header types for PHO/PHI

  // Result register
  val resultValid = RegInit(false.B)
  val resultBits = Reg(new ParseResult)

  // Initialize
  workMeta := 0.U.asTypeOf(new ParseMeta)
  headerCount := 0.U

  // Default outputs
  io.out.valid := false.B
  io.out.bits := resultBits
  io.parseDone := false.B
  io.meta := workMeta

  // Helper: shift bytes left by N bytes (dropping parsed header)
  def shiftBytes(bytes: UInt, by: UInt): UInt = {
    Mux(bytes =/= 0.U, bytes << (by * 8.U), 0.U(512.W))
  }

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
      headerTypes(headerCount) := hdrType
      headerDescs(headerCount).headerType := hdrType
      headerDescs(headerCount).offset := offset
      headerDescs(headerCount).length := length
      headerDescs(headerCount).valid := valid
      headerDescs(headerCount).errorCode := errorCode
      headerCount := headerCount + 1.U
    }
  }

  // Compute next state based on current state
  nextState := state
  switch(state) {
    is(ParserState.Idle) {
      when(io.in.valid) {
        workBytes := io.in.bits
        workMeta.totalLen := 512.U
        workMeta.parsedLen := 0.U
        workMeta.vlanCount := 0.U
        workMeta.mplsCount := 0.U
        workMeta.checksumValid := true.B
        workMeta.parseError := false.B
        workMeta.errorInfo := 0.U
        workParsedBytes := 0.U
        workNextType := ProtocolType.UNKNOWN
        resultValid := false.B
        headerCount := 0.U
        nextState := ParserState.Eth
      }
    }

    is(ParserState.Eth) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseEthernet(workBytes, workMeta)

      // Record header in PHO/PHI
      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      // Prepare interstage data
      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := !newMeta.parseError
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := !newMeta.parseError

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterEth)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        val etherType = workBytes(111, 96)
        val isVlanEtherType = etherType === EtherType.VLAN || etherType === EtherType.VLAN911
        val isMplsEtherType = etherType === EtherType.MPLS || etherType === EtherType.MPLS_UNI

        nextState := MuxLookup(etherType, ParserState.Payload)(
          Seq(
            EtherType.IPv4 -> ParserState.Ipv4,
            EtherType.IPv6 -> ParserState.Ipv6,
            EtherType.ARP  -> ParserState.Arp
          )
        )
        when(isVlanEtherType) { nextState := ParserState.Vlan }
        when(isMplsEtherType) { nextState := ParserState.Mpls }
      }.elsewhen(interstageIn.valid) {
        nextState := ParserState.Error
      }
    }

    is(ParserState.Vlan) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseVlan(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := !newMeta.parseError

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterVlan)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        val innerType = workBytes(111, 96)
        val isVlanInner = innerType === EtherType.VLAN || innerType === EtherType.VLAN911
        val isMplsInner = innerType === EtherType.MPLS || innerType === EtherType.MPLS_UNI

        nextState := ParserState.Payload
        when(innerType === EtherType.IPv4) { nextState := ParserState.Ipv4 }
        when(innerType === EtherType.IPv6) { nextState := ParserState.Ipv6 }
        when(innerType === EtherType.ARP) { nextState := ParserState.Arp }
        when(isVlanInner) { nextState := ParserState.Vlan }
        when(isMplsInner) { nextState := ParserState.Mpls }
      }
    }

    is(ParserState.Mpls) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseMpls(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterMpls)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        val bos = !workBytes(8, 8).asBool
        when(bos) {
          nextState := ParserState.Mpls
        }.otherwise {
          val ver = workBytes(139, 136)
          nextState := Mux(ver === 4.U, ParserState.Ipv4,
                       Mux(ver === 6.U, ParserState.Ipv6,
                         ParserState.Payload))
        }
      }
    }

    is(ParserState.Ipv4) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIpv4(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := !newMeta.parseError
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := !newMeta.parseError

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterIpv4)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        nextState := MuxLookup(workNextType, ParserState.Payload)(
          Seq(
            ProtocolType.TCP  -> ParserState.Tcp,
            ProtocolType.UDP  -> ParserState.Udp,
            ProtocolType.ICMP -> ParserState.Icmp,
            ProtocolType.GRE  -> ParserState.TunnelGre,
            ProtocolType.MPLS -> ParserState.Mpls
          )
        )
      }.elsewhen(interstageIn.valid) {
        nextState := ParserState.Error
      }
    }

    is(ParserState.Ipv6) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIpv6(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := !newMeta.parseError

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterIpv6)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        nextState := MuxLookup(workNextType, ParserState.Payload)(
          Seq(
            ProtocolType.TCP  -> ParserState.Tcp,
            ProtocolType.UDP  -> ParserState.Udp,
            ProtocolType.ICMP -> ParserState.Icmp,
            ProtocolType.GRE  -> ParserState.TunnelGre
          )
        )
      }
    }

    is(ParserState.Arp) {
      val (_, _, hdrLen, newMeta, errorCode, hdrType) = parseArp(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := workBytes
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := ProtocolType.UNKNOWN
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterArp)

      when(interstageOut.valid) {
        workMeta := interstageOut.bits.meta
        workParsedBytes := interstageOut.bits.parsedBytes
        nextState := ParserState.Done
      }
    }

    is(ParserState.Tcp) {
      val (_, _, hdrLen, newMeta, errorCode, hdrType) = parseTcp(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := ProtocolType.UNKNOWN
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterTcp)

      when(interstageOut.valid) {
        workMeta := interstageOut.bits.meta
        workParsedBytes := interstageOut.bits.parsedBytes
        nextState := ParserState.Done
      }
    }

    is(ParserState.Udp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseUdp(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterUdp)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        nextState := MuxLookup(workNextType, ParserState.Done)(
          Seq(
            ProtocolType.VXLAN  -> ParserState.TunnelVxlan,
            ProtocolType.GENEVE -> ParserState.TunnelGeneve,
            ProtocolType.GTPU   -> ParserState.TunnelGtpu
          )
        )
      }
    }

    is(ParserState.Icmp) {
      val (_, _, hdrLen, newMeta, errorCode, hdrType) = parseIcmp(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := workBytes
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := ProtocolType.UNKNOWN
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterIcmp)

      when(interstageOut.valid) {
        workMeta := interstageOut.bits.meta
        workParsedBytes := interstageOut.bits.parsedBytes
        nextState := ParserState.Done
      }
    }

    is(ParserState.TunnelVxlan) {
      val (_, _, hdrLen, newMeta, errorCode, hdrType) = parseVxlan(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := EtherType.IPv4
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterVxlan)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workParsedBytes := interstageOut.bits.parsedBytes
        nextState := ParserState.Eth
      }
    }

    is(ParserState.TunnelGeneve) {
      val (_, _, hdrLen, newMeta, errorCode, hdrType) = parseGeneve(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := EtherType.IPv4
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterGeneve)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workParsedBytes := interstageOut.bits.parsedBytes
        nextState := ParserState.Eth
      }
    }

    is(ParserState.TunnelGtpu) {
      val (_, _, hdrLen, newMeta, errorCode, hdrType) = parseGtpu(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := EtherType.IPv4
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterGtpu)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workParsedBytes := interstageOut.bits.parsedBytes
        val ver = workBytes(139, 136)
        nextState := Mux(ver === 4.U, ParserState.Ipv4,
                     Mux(ver === 6.U, ParserState.Ipv6,
                       ParserState.Done))
      }
    }

    is(ParserState.TunnelGre) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGre(workBytes, workMeta)

      recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)

      val interstageIn = Wire(Valid(new InterstageData))
      interstageIn.valid := true.B
      interstageIn.bits.bytes := shiftBytes(workBytes, hdrLen)
      interstageIn.bits.meta := newMeta
      interstageIn.bits.nextType := nextType
      interstageIn.bits.parsedBytes := workParsedBytes + hdrLen
      interstageIn.bits.valid := true.B

      val interstageOut = pipeAfter(interstageIn, pipelineConfig.pipeAfterGre)

      when(interstageOut.valid) {
        workBytes := interstageOut.bits.bytes
        workMeta := interstageOut.bits.meta
        workNextType := interstageOut.bits.nextType
        workParsedBytes := interstageOut.bits.parsedBytes

        nextState := MuxLookup(workNextType, ParserState.Eth)(
          Seq(
            EtherType.IPv4 -> ParserState.Ipv4,
            EtherType.IPv6 -> ParserState.Ipv6
          )
        )
      }
    }

    is(ParserState.Payload) {
      // Record payload as final header
      recordHeader(workParsedBytes, HeaderType.PAYLOAD, 0.U, true.B, HeaderErrorCode.None)
      nextState := ParserState.Done
    }

    is(ParserState.Done) {
      resultBits.fields := workBytes
      resultBits.nextType := workNextType
      resultBits.headerLen := workParsedBytes
      resultBits.valid := !workMeta.parseError
      resultBits.meta := workMeta
      resultBits.headerCount := headerCount

      // Copy PHO (header offsets) to result
      for (i <- 0 until 24) {
        resultBits.pho(i) := headerOffsets(i)
        resultBits.phi(i) := headerDescs(i)
      }

      resultValid := true.B

      io.out.valid := true.B
      io.out.bits := resultBits
      io.parseDone := true.B
      io.meta := workMeta

      nextState := ParserState.Idle
    }

    is(ParserState.Error) {
      resultBits.fields := workBytes
      resultBits.nextType := ProtocolType.UNKNOWN
      resultBits.headerLen := workParsedBytes
      resultBits.valid := false.B
      resultBits.meta := workMeta
      resultBits.headerCount := headerCount

      // Still copy PHO/PHI even on error
      for (i <- 0 until 24) {
        resultBits.pho(i) := headerOffsets(i)
        resultBits.phi(i) := headerDescs(i)
      }

      resultValid := true.B

      io.out.valid := true.B
      io.out.bits := resultBits
      io.parseDone := true.B

      nextState := ParserState.Idle
    }
  }

  // Update state
  state := nextState

  // Input ready signal
  io.in.ready := (state === ParserState.Idle) || (state === ParserState.Done) || (state === ParserState.Error)
}

// ============= Companion Object =============

object ParserCore {
  def apply(): ParserCore = Module(new ParserCore(ParserPipelineConfig.default))
  def apply(config: ParserPipelineConfig): ParserCore = Module(new ParserCore(config))
  def withAggressiveTiming(): ParserCore = Module(new ParserCore(ParserPipelineConfig.aggressiveTiming))
  def withMildTiming(): ParserCore = Module(new ParserCore(ParserPipelineConfig.mildTiming))
}