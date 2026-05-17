package FPP.Parser

import BaseCbb.GenBundle
import chisel3._

// ============= Ethernet Layer =============
class ethernet extends GenBundle{
  val da = UInt(48.W)    // Destination Address
  val sa = UInt(48.W)    // Source Address
  val etherType = UInt(16.W)  // EtherType/Length
}

// ============= VLAN =============
class vlan extends GenBundle{
  val tpid = UInt(16.W)  // Tag Protocol Identifier (0x8100 for 802.1Q, 0x88a8 for 802.1ad)
  val vlanid = UInt(12.W) // VLAN ID
  val dei = UInt(1.W)    // DEI (Drop Eligibility Indicator)
  val pri = UInt(3.W)    // Priority (PCP)
}

// QinQ (Service VLAN) - stacked VLANs
class qinq extends GenBundle{
  val outer = new vlan
  val inner = new vlan
}

// ============= MPLS =============
class mpls extends GenBundle{
  val label = UInt(20.W) // MPLS label
  val tc = UInt(3.W)     // Traffic Class
  val bos = UInt(1.W)    // Bottom of Stack
  val ttl = UInt(8.W)    // Time to Live
}

// ============= IPv4 =============
class ipv4 extends GenBundle{
  val version = UInt(4.W)
  val ihl = UInt(4.W)
  val tos = UInt(8.W)
  val length = UInt(16.W)
  val identification = UInt(16.W)
  val flags = UInt(3.W)
  val fragOffset = UInt(13.W)
  val ttl = UInt(8.W)
  val protocol = UInt(8.W)
  val checksum = UInt(16.W)
  val srcIp = UInt(32.W)
  val dstIp = UInt(32.W)
}

// ============= IPv6 =============
class ipv6 extends GenBundle{
  val version = UInt(4.W)
  val trafficClass = UInt(8.W)
  val flowLabel = UInt(20.W)
  val payloadLength = UInt(16.W)
  val nextHeader = UInt(8.W)  // Same as protocol in IPv4
  val hopLimit = UInt(8.W)
  val srcIp = UInt(128.W)
  val dstIp = UInt(128.W)
}

// ============= ARP =============
class arp extends GenBundle{
  val htype = UInt(16.W) // Hardware Type
  val ptype = UInt(16.W) // Protocol Type
  val hlen = UInt(8.W)    // Hardware Length
  val plen = UInt(8.W)   // Protocol Length
  val oper = UInt(16.W)  // Operation (1=Request, 2=Reply)
  val sha = UInt(48.W)   // Sender Hardware Address
  val spa = UInt(32.W)   // Sender Protocol Address
  val tha = UInt(48.W)   // Target Hardware Address
  val tpa = UInt(32.W)   // Target Protocol Address
}

// ============= TCP =============
class tcp extends GenBundle{
  val srcPort = UInt(16.W)
  val dstPort = UInt(16.W)
  val seqNum = UInt(32.W)
  val ackNum = UInt(32.W)
  val flags = UInt(9.W)   // NS,CWR,ECE,URG,ACK,PSH,RST,SYN,FIN
  val window = UInt(16.W)
  val checksum = UInt(16.W)
  val urgentPtr = UInt(16.W)
  val options = UInt(32.W) // Simplified: first 4 bytes of options
}

// ============= UDP =============
class udp extends GenBundle{
  val srcPort = UInt(16.W)
  val dstPort = UInt(16.W)
  val length = UInt(16.W)
  val checksum = UInt(16.W)
}

// ============= ICMP =============
class icmp extends GenBundle{
  val icmpType = UInt(8.W)
  val code = UInt(8.W)
  val checksum = UInt(16.W)
  val rest = UInt(32.W) // Varies by type/code
}

// ============= GRE =============
class gre extends GenBundle{
  val checksum = UInt(1.W)
  val routing = UInt(1.W)
  val key = UInt(1.W)
  val sequence = UInt(1.W)
  val reserved = UInt(9.W)
  val version = UInt(3.W)
  val protocol = UInt(16.W)
}

// ============= VXLAN =============
class vxlan extends GenBundle{
  val flags = UInt(8.W)
  val reserved1 = UInt(24.W)
  val vni = UInt(24.W)
  val reserved2 = UInt(8.W)
}

// ============= GTPU (GPRS Tunneling Protocol - UDP Tunnel) =============
class gtpu extends GenBundle{
  val flags = UInt(8.W)    // Version(3 bits) + PT(1 bit) + Reserved(1 bit) + E(1 bit) + S(1 bit) + PN(1 bit)
  val messageType = UInt(8.W)
  val length = UInt(16.W)
  val teid = UInt(32.W)   // Tunnel Endpoint Identifier
}

// ============= Geneve =============
class geneve extends GenBundle{
  val ver = UInt(2.W)
  val optLen = UInt(6.W)  // Options length in 4-byte units
  val oam = UInt(1.W)     // OAM packet
  val critical = UInt(1.W)
  val reserved = UInt(6.W)
  val protocol = UInt(16.W)
  val vni = UInt(24.W)
  val reserved2 = UInt(8.W)
}

// ============= NSH (Network Service Header) =============
class nsh extends GenBundle{
  val version = UInt(2.W)
  val oam = UInt(1.W)
  val reserved = UInt(1.W)
  val ttl = UInt(6.W)
  val length = UInt(6.W)
  val reserved2 = UInt(4.W)
  val mdType = UInt(4.W)
  val nextProto = UInt(8.W) // 1=NSH, 2=IPv4, 3=IPv6, 4=Eth
  val spi = UInt(24.W)      // Service Path Identifier
  val sindex = UInt(16.W)   // Service Index
}

// ============= UPF (User Plane Function) Tunnel Header =============
class upfTeid extends GenBundle{
  val teid = UInt(32.W)
  val ipVersion = UInt(1.W)
  val reserved = UInt(31.W)
}

// Common protocol numbers (for reference)
object ProtocolType {
  val ICMP = 1.U(8.W)
  val TCP = 6.U(8.W)
  val UDP = 17.U(8.W)
  val ICMPv6 = 58.U(8.W)
  val GRE = 47.U(8.W)
  val ESP = 50.U(8.W)
  val AH = 51.U(8.W)
  val OSPF = 89.U(8.W)
  val MPLS = 137.U(8.W)

  // Tunnel/Overlay protocols
  val VXLAN = 0xFF.U(8.W)      // Special marker for UDP tunnel demux
  val GENEVE = 0xFE.U(8.W)     // Special marker for UDP tunnel demux
  val GTPU = 0xFD.U(8.W)       // Special marker for UDP tunnel demux
  val NSH = 0xFC.U(8.W)        // Network Service Header
  val UNKNOWN = 0.U(8.W)
}

// EtherType values (for reference)
object EtherType {
  val IPv4 = 0x0800.U(16.W)
  val IPv6 = 0x86DD.U(16.W)
  val ARP = 0x0806.U(16.W)
  val VLAN = 0x8100.U(16.W)
  val VLAN911 = 0x88a8.U(16.W)  // 802.1ad (QinQ)
  val MPLS = 0x8847.U(16.W)
  val MPLS_UNI = 0x8848.U(16.W)
  val LLDP = 0x88CC.U(16.W)
  val Ethernet = 0x6558.U(16.W) // Internal use -Ethernet II without length/type
}

