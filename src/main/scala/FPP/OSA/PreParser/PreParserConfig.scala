package FPP.OSA.PreParser

import BaseCbb.GenBundle

// ============= PreParser Configuration =============

case class PreParserConfig(
  portCount: Int = 16,
  bytesToParse: Int = 32,
  tcamDepth: Int = 16,
  maxVlanLayers: Int = 3
)

object PreParserConfig {
  def default = PreParserConfig()
  def withVlanLayers(n: Int) = PreParserConfig(maxVlanLayers = n)
}

// ============= Trust Mode =============

object TrustMode extends ChiselEnum {
  val VLAN = 0.U(2.W)      // 00 = Trust VLAN priority
  val DSCP = 1.U(2.W)      // 01 = Trust DSCP priority
  val OPAQUE = 2.U(2.W)     // 10 = Trust OpaqueTag priority
  val RESERVED = 3.U(2.W)   // 11 = Reserved
}

// ============= Port Configuration =============

class PortConfig extends GenBundle {
  val trustMode = UInt(2.W)    // 00=VLAN, 01=DSCP, 10=OpaqueTag, 11=Reserved
  val tcamEnable = Bool()       // Enable TCAM override
  val defaultPri = UInt(4.W)    // Default priority when no source available
}

object PortConfig {
  def default = {
    val cfg = Wire(new PortConfig)
    cfg.trustMode := TrustMode.VLAN
    cfg.tcamEnable := false.B
    cfg.defaultPri := 0.U(4.W)
    cfg
  }
}

// ============= Error Codes =============

object PreParserErrorCode extends ChiselEnum {
  val None = 0.U(4.W)
  val NoVlanNoIpNoOpaque = 1.U(4.W)
  val VlanTcamMiss = 2.U(4.W)
  val DscpTcamMiss = 3.U(4.W)
  val OpaqueTcamMiss = 4.U(4.W)
  val InvalidEtherType = 5.U(4.W)
  val VlanOverflow = 6.U(4.W)
  val InvalidOpaqueFormat = 7.U(4.W)
}

// ============= Constants =============

object PreParserConstants {
  // EtherType values
  val ETH_VLAN = 0x8100.U(16.W)
  val ETH_VLAN911 = 0x88a8.U(16.W)  // 802.1ad (QinQ)
  val ETH_IPV4 = 0x0800.U(16.W)
  val ETH_IPV6 = 0x86DD.U(16.W)
  val ETH_OPAQUE = 0xFFFF.U(16.W)

  // VLAN TPID values
  val VLAN_TPID_8100 = 0x8100.U(16.W)
  val VLAN_TPID_88A8 = 0x88a8.U(16.W)

  // OpaqueTag format
  val OPAQUE_FORMAT_CUSTOM_PRI = 0x1.U(4.W)

  // Byte offsets within 32B packet data
  val OFFSET_DMAC = 0
  val OFFSET_SMAC = 6
  val OFFSET_ETYPE = 12
  val OFFSET_VLAN1_TCI = 16
  val OFFSET_VLAN2_TCI = 20
  val OFFSET_VLAN3_TCI = 24
  val OFFSET_OPAQUE = 26
  val OFFSET_IPV4 = 14

  // Max VLAN layers
  val MAX_VLAN_LAYERS = 3
}