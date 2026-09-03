package FPP.Parser

import BaseCbb.data.GenBundle
import chisel3._

/**
 * Parser 公共类型：协议/错误编码、解析中间表示、状态编码。
 *
 * ==字节序约定==
 * 本模块采用 **网络序（首字节在高位）**：报文第 k 个字节位于 `bits(511-8k, 504-8k)`，
 * 即 `word = Cat(byte0, byte1, ..., byte63)`。
 * 于是"跳过当前头部 N 字节" = **逻辑左移** `8*N` 位，下一个头部的第 0 字节回到最高位段。
 * 所有 parseXxx 函数都假设入参 `bytes` 的最高位段就是本层头部的第 0 字节。
 *
 * 大端字段（如 EtherType、IP total length、UDP 端口）在报文里高字节在前，
 * 打包后高字节同样落在更高的位段，因此可直接按数值读出，无需逐字段字节反转：
 * 从字节 k 起、宽 w 字节的字段 = `bytes(511 - 8*k, 512 - 8*(k + w))`。
 * 例如 EtherType 位于第 12/13 字节 → `bytes(415, 400)`。
 */

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
// NOTE: plain object, not ChiselEnum -- the codes are raw UInt literals so they
// can be mixed freely with Mux/MuxLookup and stored in a UInt<4> field.
object HeaderErrorCode {
  val None                = 0.U(4.W)
  val InvalidEtherType    = 1.U(4.W)
  val Ipv4ChecksumError   = 2.U(4.W)
  val InvalidProtocol     = 3.U(4.W)
  val TruncatedHeader     = 4.U(4.W)
  val InvalidHeaderLength = 5.U(4.W)
  val VlanCountOverflow   = 6.U(4.W)
  val MplsCountOverflow   = 7.U(4.W)
  val TunnelNotSupported  = 8.U(4.W)
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
  val offset = UInt(16.W)       // Byte offset from packet start
  val length = UInt(8.W)        // Header length in bytes
  val valid = Bool()            // Header parsed successfully
  val errorCode = UInt(4.W)     // Error code if valid=false
}

// ============= Parse Metadata =============
class ParseMeta extends GenBundle {
  val totalLen = UInt(16.W)       // Total packet length
  val parsedLen = UInt(16.W)      // Bytes parsed so far
  val vlanCount = UInt(3.W)       // Number of VLAN tags parsed
  val mplsCount = UInt(4.W)       // Number of MPLS labels
  val checksumValid = Bool()      // Checksum validation result
  val parseError = Bool()         // Any parse error
  val errorInfo = UInt(4.W)       // Error code for debugging
}

// ============= Parse Result =============
class ParseResult extends GenBundle {
  val fields = UInt(512.W)        // Extracted fields
  val nextType = UInt(8.W)        // Next protocol type
  val headerLen = UInt(8.W)       // Current header length in bytes
  val valid = Bool()              // Parsing valid
  val meta = new ParseMeta        // Metadata pass-through

  // Packet Header Offset array (max 24 headers)
  val pho = Vec(24, UInt(16.W))
  // Packet Header Information array
  val phi = Vec(24, new PacketHeaderDesc)
  // Number of headers parsed
  val headerCount = UInt(5.W)
}

// ============= Parser States =============
// NOTE: see HeaderErrorCode -- plain object, raw UInt literals.
object ParserState {
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

// ============= Interstage Data Bundle =============
/** Data passed between parser stages. */
class InterstageData extends GenBundle {
  val bytes = UInt(512.W)
  val meta = new ParseMeta
  val nextType = UInt(8.W)
  val parsedBytes = UInt(16.W)
  val valid = Bool()
}
