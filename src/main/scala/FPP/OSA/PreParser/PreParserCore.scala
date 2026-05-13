package FPP.OSA.PreParser

import BaseCbb.GenModule
import BaseCbb.GenBundle
import chisel3._
import chisel3.util._

class PreParserCore(
  val config: PreParserConfig = PreParserConfig()
) extends GenModule {

  val io = IO(new Bundle {
    // Input from external packet data (first 32 bytes)
    val in_data = Input(UInt(256.W))
    val in_portId = Input(UInt(4.W))
    val in_valid = Input(Bool())

    // Port configuration
    val portConfig = Input(Vec(config.portCount, new PortConfig))

    // TCAM entries (one per port)
    val tcamEntries = Input(Vec(config.tcamDepth, new TcamEntry))

    // LUTs: VLAN priority LUT (128 entries), DSCP priority LUT (512 entries), Opaque priority LUT (256 entries)
    val vlanPrioLut = Input(Vec(128, UInt(4.W)))
    val dscpPrioLut = Input(Vec(512, UInt(4.W)))
    val opaquePrioLut = Input(Vec(256, UInt(4.W)))

    // Output
    val out_priority = Output(UInt(4.W))
    val out_valid = Output(Bool())
  })

  // ========== VLAN Extraction ==========

  def extractVlanPrio(data: UInt, offset: UInt): (Bool, UInt, UInt) = {
    val tpid = data(offset + 1, offset)  // TPID at offset
    val isVlan = (tpid === PreParserConstants.ETH_VLAN) || (tpid === PreParserConstants.ETH_VLAN911)

    val tci = data(offset + 5, offset + 4)  // TCI after TPID (4 bytes offset)
    val pri = tci(2, 0)                     // PCP/Priority (3 bits)
    val dei = tci(4)                         // DEI (1 bit)
    val vid = tci(15, 3)                     // VLAN ID (12 bits)

    val vlanPrio = Cat(dei, pri)             // 4-bit: {DEI, PRI[2:0]}

    (isVlan, vlanPrio, vid)
  }

  // VLAN extraction with up to 3 layers
  val vlanResult = Wire(new VlanExtractResult)
  vlanResult.vlanCount := 0.U(2.W)
  vlanResult.vlanPrio := 0.U(4.W)
  vlanResult.vlanVid := 0.U(12.W)
  vlanResult.hasOpaqueTag := false.B
  vlanResult.hasIp := false.B
  vlanResult.dscp := 0.U(6.W)

  // Sequential VLAN layer detection - check each layer based on previous result
  // Layer 1: Check at offset 12 (after DMAC+SMAC, EtherType at 12-13)
  val (isVlan1, vlanPrio1, vid1) = extractVlanPrio(io.in_data, 12.U)

  // Layer 2: Check at offset 16 (after VLAN1) - only if Layer 1 was VLAN
  val (isVlan2, vlanPrio2, vid2) = extractVlanPrio(io.in_data, 16.U)

  // Layer 3: Check at offset 20 (after VLAN2) - only if Layer 2 was VLAN
  val (isVlan3, vlanPrio3, vid3) = extractVlanPrio(io.in_data, 20.U)

  // Build VLAN count and priority based on detected layers
  // Outermost VLAN (first one detected) provides the priority
  when(isVlan1) {
    vlanResult.vlanCount := 1.U(2.W)
    vlanResult.vlanPrio := vlanPrio1
    vlanResult.vlanVid := vid1

    when(isVlan2) {
      vlanResult.vlanCount := 2.U(2.W)
      // Priority comes from outermost (VLAN1), not inner

      when(isVlan3) {
        vlanResult.vlanCount := 3.U(2.W)
      }
    }
  }

  // Determine next EtherType after VLAN layers for OpaqueTag/IP detection
  // EtherType offset depends on VLAN count
  val nextEtherTypeOffset = MuxLookup(vlanResult.vlanCount, 12.U)(
    Seq(
      0.U -> 12.U,
      1.U -> 16.U,
      2.U -> 20.U,
      3.U -> 24.U
    )
  )
  val etherTypeAfterVlan = io.in_data(nextEtherTypeOffset + 1, nextEtherTypeOffset)

  vlanResult.hasOpaqueTag := etherTypeAfterVlan === PreParserConstants.ETH_OPAQUE
  vlanResult.hasIp := (etherTypeAfterVlan === PreParserConstants.ETH_IPV4) ||
                       (etherTypeAfterVlan === PreParserConstants.ETH_IPV6)

  // ========== OpaqueTag Extraction ==========

  /**
   * OpaqueTag format:
   * - 4B format: bits[3:0]=format, bits[7:4]=pri, bits[31:8]=reserved
   * - 8B format: bits[3:0]=format, bits[7:4]=pri, bits[31:8]=reserved, bits[63:32]=more_data
   * Format value 0x1 = custom priority, other values reserved
   * Length: 0 = 4B (1 unit), 1 = 8B (2 units)
   */
  def extractOpaqueTag(data: UInt, offset: UInt): OpaqueExtractResult = {
    val result = Wire(new OpaqueExtractResult)

    // OpaqueTag starts at offset (EtherType=0xFFFF at offset, so OpaqueTag data at offset+2)
    val format = data(offset + 3, offset + 2)(3, 0)  // bits[27:24] = format[3:0]
    val pri = data(offset + 3, offset + 2)(7, 4)     // bits[31:28] = pri[3:0]

    // For 4B format: no additional length check needed
    // For 8B format: we just indicate length, actual parsing of extra data not needed for priority
    val is4B = true.B  // Default assumption
    val length = Mux(is4B, 0.U(2.W), 1.U(2.W))  // 0 = 4B, 1 = 8B

    // OpaqueTag is valid only if format = 0x1 (custom priority)
    result.isValid := (format === PreParserConstants.OPAQUE_FORMAT_CUSTOM_PRI)
    result.format := format
    result.length := length
    result.priority := pri

    result
  }

  // OpaqueTag offset depends on VLAN layers
  // After VLAN layers, next header EtherType is at offset 12 + 4*vlanCount
  // OpaqueTag content starts 2 bytes after EtherType
  val nextHeaderOffset = Cat(0.U(2.W), 12.U(6.W)) + (vlanResult.vlanCount << 2)(7, 0)
  val opaqueOffset = nextHeaderOffset + 2.U

  val opaqueResult = extractOpaqueTag(io.in_data, opaqueOffset)

  // ========== DSCP Extraction ==========

  def extractDscpFromIpv4(data: UInt, offset: UInt): UInt = {
    // IPv4 header: offset points to version/IHL byte
    // DSCP is at bits[47:42] relative to offset (after version/IHL and TOS bytes)
    val dscp = data(offset + 3, offset + 2)(5, 0)  // TOS byte lower 6 bits
    dscp
  }

  def extractDscpFromIpv6(data: UInt, offset: UInt): UInt = {
    // IPv6: offset points to version byte, DSCP is at offset+1 in traffic class field
    val tc = data(offset + 9, offset + 8)  // traffic class bytes
    val dscp = tc(5, 0)  // lower 6 bits
    dscp
  }

  val dscpResult = Wire(new DscpExtractResult)
  dscpResult.isValid := false.B
  dscpResult.dscp := 0.U(6.W)

  // DSCP offset depends on VLAN layers (IP starts at offset 14 + 4*n VLAN layers)
  val ipOffset = MuxCase(
    14.U,  // No VLAN: IP at 14
    Seq(
      (vlanResult.vlanCount === 1.U) -> 18.U,
      (vlanResult.vlanCount === 2.U) -> 22.U,
      (vlanResult.vlanCount === 3.U) -> 26.U
    )
  )

  // Check if it's IPv4 or IPv6 and extract DSCP
  val ipEtherType = MuxCase(
    io.in_data(31, 16),
    Seq(
      (vlanResult.vlanCount === 1.U) -> io.in_data(31, 16),
      (vlanResult.vlanCount === 2.U) -> io.in_data(47, 32),
      (vlanResult.vlanCount === 3.U) -> io.in_data(63, 48)
    )
  )

  when(ipEtherType === PreParserConstants.ETH_IPV4) {
    dscpResult.isValid := true.B
    dscpResult.dscp := extractDscpFromIpv4(io.in_data, ipOffset)
  }.elsewhen(ipEtherType === PreParserConstants.ETH_IPV6) {
    dscpResult.isValid := true.B
    dscpResult.dscp := extractDscpFromIpv6(io.in_data, ipOffset)
  }

  // ========== TCAM Matching ==========

  def tcamMatch(data: UInt, entry: TcamEntry): Bool = {
    val dmac = data(47, 0)
    val smac = data(95, 48)

    val dmacMatch = ((dmac ^ entry.dmacValue) & entry.dmacMask) === 0.U
    val smacMatch = ((smac ^ entry.smacValue) & entry.smacMask) === 0.U

    entry.valid && dmacMatch && smacMatch
  }

  val tcamHit = Wire(Bool())
  val tcamPriority = Wire(UInt(4.W))

  val portConfig = io.portConfig(io.in_portId)
  val tcamEntry = io.tcamEntries(io.in_portId)

  tcamHit := false.B
  tcamPriority := 0.U(4.W)

  when(portConfig.tcamEnable) {
    val hit = tcamMatch(io.in_data, tcamEntry)
    tcamHit := hit
    tcamPriority := tcamEntry.priority
  }

  // ========== Priority LUT Lookup ==========

  // VLAN priority LUT key: {portId[3:0], vlanPrio[3:0]} = 7 bits = 128 entries
  val vlanLutKey = Cat(io.in_portId, vlanResult.vlanPrio)(6, 0)
  val vlanLutPriority = Mux(vlanLutKey < 128.U, io.vlanPrioLut(vlanLutKey), 0.U(4.W))

  // DSCP priority LUT key: {portId[3:0], dscp[5:0]} = 9 bits = 512 entries
  val dscpLutKey = Cat(io.in_portId, dscpResult.dscp)(8, 0)
  val dscpLutPriority = Mux(dscpLutKey < 512.U, io.dscpPrioLut(dscpLutKey), 0.U(4.W))

  // OpaqueTag priority LUT key: {portId[3:0], opaquePrio[3:0]} = 7 bits = 256 entries
  val opaqueLutKey = Cat(io.in_portId, opaqueResult.priority)(6, 0)
  val opaqueLutPriority = Mux(opaqueLutKey < 256.U, io.opaquePrioLut(opaqueLutKey), 0.U(4.W))

  // ========== Priority Selection ==========

  val priorityResult = Wire(new PriorityResult)

  // TCAM hit takes highest priority
  when(tcamHit) {
    priorityResult.priority := tcamPriority
    priorityResult.source := 1.U(3.W)  // TCAM
    priorityResult.valid := true.B
  }.elsewhen(portConfig.trustMode === TrustMode.VLAN) {
    // Trust VLAN
    when(vlanResult.vlanCount =/= 0.U) {
      priorityResult.priority := vlanLutPriority
      priorityResult.source := 2.U(3.W)  // VLAN
      priorityResult.valid := true.B
    }.otherwise {
      priorityResult.priority := portConfig.defaultPri
      priorityResult.source := 0.U(3.W)  // Default
      priorityResult.valid := true.B
    }
  }.elsewhen(portConfig.trustMode === TrustMode.DSCP) {
    // Trust DSCP
    when(dscpResult.isValid) {
      priorityResult.priority := dscpLutPriority
      priorityResult.source := 3.U(3.W)  // DSCP
      priorityResult.valid := true.B
    }.otherwise {
      priorityResult.priority := portConfig.defaultPri
      priorityResult.source := 0.U(3.W)  // Default
      priorityResult.valid := true.B
    }
  }.elsewhen(portConfig.trustMode === TrustMode.OPAQUE) {
    // Trust OpaqueTag
    when(opaqueResult.isValid) {
      priorityResult.priority := opaqueLutPriority
      priorityResult.source := 4.U(3.W)  // OpaqueTag
      priorityResult.valid := true.B
    }.otherwise {
      priorityResult.priority := portConfig.defaultPri
      priorityResult.source := 0.U(3.W)  // Default
      priorityResult.valid := true.B
    }
  }.otherwise {
    // Reserved trust mode - use default
    priorityResult.priority := portConfig.defaultPri
    priorityResult.source := 0.U(3.W)  // Default
    priorityResult.valid := true.B
  }

  // ========== Output ==========

  io.out_priority := Mux(io.in_valid, priorityResult.priority, 0.U(4.W))
  io.out_valid := io.in_valid && priorityResult.valid
}

object PreParserCore {
  def apply(): PreParserCore = Module(new PreParserCore(PreParserConfig()))
  def apply(config: PreParserConfig): PreParserCore = Module(new PreParserCore(config))
}