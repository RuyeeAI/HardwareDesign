package em

import chisel3._
import chisel3.util.log2Ceil
import BaseCbb.memory.MemoryProtectType
// Enumeration 的类型别名与对象同名，需要分别导入（对象给枚举值，别名给类型位置）
import BaseCbb.memory.MemoryProtectType.MemoryProtectType

// ===========================================================================
// EM（Exact Match）模块的参数与位宽布局
//
// 术语（交换芯片 EM 引擎的常见三级组织）：
//   HT  Hash Table     —— 哈希桶，存"指向 KT 的指针"
//   KT  Key Table      —— 存完整 key（+ 指向 AD 的指针或内联的 AD）
//   AD  Action Data    —— 存动作数据（查表结果负载）
//
// d-left：numBanks 个哈希子表，插入时选"占用最少"的子表（d-left hashing），
//   显著提高装填率。numBanks = 1 即退化为普通组相联哈希表。
//
// 【流水线版布局要点】（相对串行版）
//   1. KT 按 (bank, way) 分成 ktBanks = numBanks*ways 个物理 bank：
//      插入桶 (b) 路 (w) 的条目固定从 KT bank (b*ways+w) 分配，
//      因此查找时 ktBanks 个候选必然落在不同 bank → 一次全并行读，
//      不再逐候选串行扫描（这是 II=1 的前提）。
//      每个 slot 的容量 = ktDepth/ktBanks，不再全局共享。
//   2. HT 的 SRAM 只存 payload；valid / 时间戳 / claim 位放在 HT 旁的
//      寄存器阵列（AgeTable）：查找时 valid 组合读出，老化扫描只扫寄存器，
//      **扫描不占访存带宽**。
//   3. AD 从 SP 改为 TP：维护写 AD 与流水线读 AD 不再互斥。
//      同理 HT/KT 都是 TP（同拍 1 读 + 1 写），因此**只有读需要时隙**。
// ===========================================================================

sealed trait CrcMode {
  def poly: BigInt
  def init: BigInt
  def xorout: BigInt
  def refin: Boolean
  def refout: Boolean
}

/** 固化 CRC：多项式在 elaboration 期已知 → 展开为纯 XOR 树，组合逻辑、0 拍、无配置寄存器。 */
final case class CrcHardwired(
    poly: BigInt,
    init: BigInt,
    xorout: BigInt,
    refin: Boolean,
    refout: Boolean
) extends CrcMode

object CrcHardwired {
  /** CRC-32 常用形态（以太网 FCS）：poly=0x04C11DB7，init/xorout=0xFFFFFFFF，输入输出均反射 */
  def crc32: CrcHardwired =
    CrcHardwired(BigInt("04C11DB7", 16), BigInt("FFFFFFFF", 16), BigInt("FFFFFFFF", 16), true, true)
  def apply(poly: BigInt): CrcHardwired = CrcHardwired(poly, 0, 0, false, false)
}

/**
 * 运行时可配 CRC：poly/init/xorout 由 CSR 改写，硬件是串行 LFSR。
 *
 * ⚠️ 串行 CRC 必然占 keyWidth 拍，S0 会被 stall 住，**此模式下无法 II=1**
 * （II=1 仅对 CrcHardwired 成立）。见设计文档 §12。
 */
final case class CrcRuntime(
    poly: BigInt,
    init: BigInt,
    xorout: BigInt,
    refin: Boolean,
    refout: Boolean
) extends CrcMode

object CrcRuntime {
  def crc32: CrcRuntime =
    CrcRuntime(BigInt("04C11DB7", 16), BigInt("FFFFFFFF", 16), BigInt("FFFFFFFF", 16), true, true)
}

/**
 * 老化参数（时间戳法：命中刷新时间戳，后台扫描器找超时条目）。
 *
 * @param ageWidth   时间戳位宽（决定最大可表达超时）
 * @param timeout    超时阈值，单位为 tick（now 每 tickDiv 拍 +1）
 * @param tickDiv    now 自增的分频
 * @param sweepEnable 是否内置扫描器；false 时只维护时间戳，由外部读状态自行删除
 */
final case class AgingParams(ageWidth: Int = 16, timeout: Int = 1024, tickDiv: Int = 64, sweepEnable: Boolean = true)

/**
 * 自学习参数（查表 miss 时自动插入条目）。
 *
 * @param defaultAd  learnAd 端口不可用时的兜底动作数据
 * @param usePortAd  true = 用 io.learnAd 端口的值（L2 学习场景即入端口）
 */
final case class LearningParams(defaultAd: BigInt = BigInt(0), usePortAd: Boolean = true)

/**
 * d-left 的并列仲裁策略：多个子表占用相同时选哪个。
 *
 * - Leftmost：取最左。最简单，但轻载时几乎每次都并列为 0 → 条目全堆在第一个子表，
 *   **d-left 的负载均衡完全失效**（实测：9 条条目全部落在 bank0）。
 * - Random：经典 d-left 的做法，用 LFSR 随机挑一个并列者，装填率最高。
 * - RoundRobin：轮转。确定性、且保证各子表均匀，便于验证。
 */
sealed trait TiePolicy
object TiePolicy {
  case object Leftmost   extends TiePolicy
  case object Random     extends TiePolicy
  case object RoundRobin extends TiePolicy
}

final case class EmParams(
    keyWidth: Int = 48,      // 查找 key 位宽（如 MAC 48 位）
    adWidth: Int = 32,       // 动作数据位宽（查表结果）
    htDepth: Int = 1024,     // HT 总桶数（会被 numBanks 均分；须为 2 的幂）
    htWays: Int = 4,         // 每桶路数（组相联度）
    numBanks: Int = 1,       // d-left 子表数；1 = 关闭 d-left；2 = 2-left
    dLeftTie: TiePolicy = TiePolicy.Random,  // 并列时的仲裁（见 TiePolicy 注释）
    ktDepth: Int = 4096,     // KT 总条目数（= ktBanks 个 slot 的总和；须为 2 的幂）
    useKt: Boolean = true,   // false = key 内联进 HT（单级表，查找最快）
    adDepth: Int = 1024,     // AD 条目数（须为 2 的幂）
    useAd: Boolean = true,   // false = 动作数据内联进 KT（省一级间接）
    ovfcDepth: Int = 0,      // OVFC：HT 溢出 TCAM 深度；0 = 关闭；须为 2 的幂（0 除外）
    crcWidth: Int = 32,      // CRC 输出位宽
    crc: CrcMode = CrcHardwired.crc32,
    learnFwdDepth: Int = 8,  // 在途插入转发 CAM 深度（覆盖"插入落盘"前的在途查找）
    slotWaitMax: Int = 8,    // svc 等待空时隙的上限拍数，超时则向前反压换取时隙
                             // （越小＝维护/老化越跟得上，代价是查找 II 的少量损失）
    aging: Option[AgingParams] = None,
    learning: Option[LearningParams] = None,
    memProtect: MemoryProtectType = MemoryProtectType.ProtNone  // 存储保护（透传 Memory.scala）
)

/** 由参数推导出的全部位宽与条目布局。所有字段访问都走这里，避免各处重复推导。 */
final case class EmLayout(p: EmParams) {
  require(p.keyWidth > 0 && p.adWidth > 0, "keyWidth/adWidth 必须 > 0")
  require(p.numBanks >= 1, "numBanks 必须 >= 1（1 = 关闭 d-left）")
  require(p.htDepth % p.numBanks == 0, s"htDepth(${p.htDepth}) 必须能被 numBanks(${p.numBanks}) 整除")
  require(p.htWays >= 1, "htWays 必须 >= 1")
  require(p.ovfcDepth >= 0, "ovfcDepth 必须 >= 0（0 = 关闭 OVFC）")
  require(p.learnFwdDepth >= 1, "learnFwdDepth 必须 >= 1")
  require(p.slotWaitMax >= 1, "slotWaitMax 必须 >= 1")
  // Memory.scala 的 addrWidth = log2Ceil(depth)，非 2 的幂深度会产生越界地址
  require(pow2(p.htDepth) && pow2(p.ktDepth) && pow2(p.adDepth) && pow2(p.htDepth / p.numBanks),
    "htDepth / ktDepth / adDepth / bankDepth 必须是 2 的幂（Memory.scala 的地址位宽按 log2Ceil 推导）")

  private def pow2(n: Int): Boolean = n > 0 && (n & (n - 1)) == 0

  val numBanks = p.numBanks
  val ways     = p.htWays
  val keyW     = p.keyWidth
  val adW      = p.adWidth

  val bankDepth = p.htDepth / p.numBanks
  val idxW      = math.max(1, log2Ceil(bankDepth))
  val bankW     = math.max(1, log2Ceil(numBanks))
  val wayW      = math.max(1, log2Ceil(ways))            // 路编号位宽（ways=1 时取 1，避免 0 宽）
  val slotW     = math.max(1, log2Ceil(numBanks * ways)) // (bank,way) 扁平槽位编号位宽
  val ktBanks   = numBanks * ways   // KT 的物理 bank 数 = 一次查找并行读的候选数
  val hashW     = idxW * numBanks
  require(hashW <= p.crcWidth, s"hash 需要 ${hashW} 位，超过 crcWidth(${p.crcWidth})")

  val crcW = p.crcWidth

  // ---- KT：按 (bank,way) 分 bank ----
  // 插入桶 (b) 的路 (w) 时，KT 条目固定从 bank (b*ways+w) 分配，
  // 因此查找时 ktBanks 个候选落在不同 bank，可一次全并行读。
  // 代价：每个 slot 的容量独立（不再是全局共享的空闲池），必须够用：
  //   单 slot 上的条目数 <= bankDepth（每桶最多 1 条）+ ovfcDepth（OVFC 也可能落在这个 slot）
  val ktDepthReal = if (p.useKt) p.ktDepth / ktBanks else 1
  val ktPtrW      = math.max(1, log2Ceil(ktDepthReal))
  if (p.useKt) {
    require(p.ktDepth % ktBanks == 0, s"ktDepth(${p.ktDepth}) 必须能被 ktBanks($ktBanks) 整除")
    require(ktDepthReal >= bankDepth + p.ovfcDepth,
      s"KT 每个 slot 容量 ktDepth/ktBanks=${ktDepthReal} 不足：需要 >= bankDepth(${bankDepth}) + ovfcDepth(${p.ovfcDepth})")
  }

  val adPtrW  = math.max(1, log2Ceil(p.adDepth))

  // ---- 老化 ----
  val ageW    = p.aging.map(_.ageWidth).getOrElse(0)
  val ageW1   = math.max(1, ageW)
  val sweepOn = p.aging.exists(_.sweepEnable)
  val agingOn = p.aging.isDefined

  // ---- HT：SRAM 只存 payload，valid/ts/claim 在 AgeTable ----
  //   useKt  : 负载 = ktPtr（指向本 slot 的 KT bank）
  //   !useKt : 负载 = key + (useAd ? adPtr : ad)
  val htPayW   = if (p.useKt) ktPtrW else (keyW + (if (p.useAd) adPtrW else adW))
  val htWordW  = htPayW * ways          // 一个 HT word = 一整桶（ways 条 payload）
  val ageEntryW = 1 + ageW + 1          // valid + ts + claim
  val ageWords = p.htDepth * ways       // 老化阵列总条目数（= HT 总条目数）

  // ---- KT 条目: [keyW-1:0]=key, 高位 = (useAd ? adPtr : ad) ----
  val ktPayW   = if (p.useAd) adPtrW else adW
  val ktEntryW = keyW + ktPayW

  // ---- OVFC（HT 溢出 TCAM）----
  // useKt 时负载 = {ktSlot, ktPtr}：KT 按 slot 分 bank，而 OVFC 条目不在某个固定的
  // (bank,way) 位置上，必须自己记住它的 KT 落在哪个 bank，否则取不到 KT。
  val ovfcPayW = if (p.useKt) ktPtrW + slotW else (if (p.useAd) adPtrW else adW)
  val ovfcEn   = p.ovfcDepth > 0
  // 注意区分"选择位宽"和"计数位宽"：计数必须能表达 ovfcDepth 本身（=扫描结束），
  // 否则扫描游标永远到不了终点（实测踩过）。
  val ovfcSelW = math.max(1, log2Ceil(math.max(1, p.ovfcDepth)))
  val ovfcCntW = math.max(1, log2Ceil(p.ovfcDepth + 1))

  val rspW     = 1 + adW          // 对外响应：hit(最高位) + ad

  /**
   * 查找延迟（拍，从接受请求到 rsp 有效）：
   *   S1 接收（哈希打拍）+ HT 读 + KT 读 + AD 读 + S4 输出
   * 不含 CrcRuntime 串行 CRC 的 keyWidth 拍（那种模式下无法 II=1）。
   */
  val lookupLatency = 2 + (if (p.useKt) 1 else 0) + (if (p.useAd) 1 else 0)

  // ---- HT payload 字段访问器 ----
  def htKtPtr(e: UInt): UInt = { require(p.useKt); e(ktPtrW - 1, 0) }
  def htKey(e: UInt): UInt = { require(!p.useKt); e(keyW - 1, 0) }
  def htAdPtr(e: UInt): UInt = { require(!p.useKt && p.useAd); e(htPayW - 1, keyW) }
  def htAd(e: UInt): UInt = { require(!p.useKt && !p.useAd); e(htPayW - 1, keyW) }

  // ---- OVFC 负载字段访问器 ----
  def ovfcKtSlot(e: UInt): UInt = { require(p.useKt); e(ovfcPayW - 1, ktPtrW) }
  def ovfcKtPtr(e: UInt): UInt = { require(p.useKt); e(ktPtrW - 1, 0) }

  // ---- KT 条目字段访问器 ----
  def ktKey(e: UInt): UInt = e(keyW - 1, 0)
  def ktAdPtr(e: UInt): UInt = { require(p.useAd); e(ktEntryW - 1, keyW) }
  def ktAd(e: UInt): UInt = { require(!p.useAd); e(ktEntryW - 1, keyW) }
  def ktEntry(key: UInt, pay: UInt): UInt = chisel3.util.Cat(pay, key)

  // 注意：ways == 1 时路编号占 0 位，所有"按路拆位"的地方都要单独处理，
  // 否则会多出一位偏移（ways=1 时 s = b，不能写成 s >> 1）。
  private val wayBits = log2Ceil(ways)   // ways=1 时为 0

  /** 扁平槽位 s = b*ways + w 拆回 (bank, way)；s 同时也是 KT 的物理 bank 号 */
  def slotBank(s: UInt): UInt = if (numBanks > 1) s(slotW - 1, wayBits) else 0.U
  def slotWay(s: UInt): UInt  = if (wayBits > 0) s(wayBits - 1, 0) else 0.U
}
