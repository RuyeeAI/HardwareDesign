package P4C

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import p4cgen._

/** 运行时表端到端测试：demo7-runtime-table.p4 → Demo7RuntimeTableIngress
  * （静态表 static_table + 运行时表 rt_table 共存）。
  *
  * 条目位布局（MSB → LSB，与生成文件头注释一致）：
  * valid(1) | actionId(2) | args(24) | key(16) = 43 位；size=6 ⇒ addrW=3。
  * action 编号按 P4 `actions = { set_cls; set_port; nop; }` 声明序：0/1/2。
  */
class Demo7RuntimeTableSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  private val KeyBits = 16
  private val ArgW = 24
  private val EntryW = 43

  /** 条目编码：valid 位 + actionId + 参数位串 + key。 */
  private def entry(act: Int, key: Int, args: BigInt = 0, valid: Boolean = true): BigInt =
    (if (valid) BigInt(1) << (EntryW - 1) else BigInt(0)) |
      (BigInt(act) << (ArgW + KeyBits)) |
      (args << KeyBits) | BigInt(key)

  // set_cls(c)：单参数，占参数位串最低 8 位（off=0）
  private def argsCls(c: Int): BigInt = BigInt(c)

  // set_port(p, t)：p 先声明占高位（off = t 的宽度 8），t 占最低 8 位
  private def argsPort(p: Int, t: Int): BigInt = (BigInt(p) << 8) | BigInt(t)

  private def writeEntry(c: Demo7RuntimeTableIngress, addr: Int, data: BigInt): Unit = {
    c.io.tbl_rt_table_we.poke(true.B)
    c.io.tbl_rt_table_waddr.poke(addr.U(3.W))
    c.io.tbl_rt_table_wdata.poke(data.U(EntryW.W))
    c.clock.step(1)
    c.io.tbl_rt_table_we.poke(false.B)
  }

  /** 驱动输入：默认 metaIn = (cls=7, normPort=0x1234, tag=0xab, stat=1)。 */
  private def pokeIn(c: Demo7RuntimeTableIngress, etherType: Int, srcAddr: BigInt = BigInt(0x02)): Unit = {
    c.io.hdrIn.ethernet.dstAddr.poke(0.U(48.W))
    c.io.hdrIn.ethernet.srcAddr.poke(srcAddr.U(48.W))
    c.io.hdrIn.ethernet.etherType.poke(etherType.U(16.W))
    c.io.metaIn.cls.poke(7.U(8.W))
    c.io.metaIn.normPort.poke(0x1234.U(16.W))
    c.io.metaIn.tag.poke(0xab.U(8.W))
    c.io.metaIn.stat.poke(1.U(8.W))
  }

  behavior.of("Demo7RuntimeTableIngress（由 demo7-runtime-table.p4 生成）")

  it should "① 上电空表全 miss：走 default(nop) 字段透传" in {
    test(new Demo7RuntimeTableIngress) { c =>
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(7.U(8.W))
      c.io.metaOut.normPort.expect(0x1234.U(16.W))
      c.io.metaOut.tag.expect(0xab.U(8.W))
    }
  }

  it should "② 写入后命中：执行对应 action 与参数（set_cls(3)）" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(3.U(8.W))
      // 未命中 key → 仍是 default 透传
      pokeIn(c, 0x86dd)
      c.io.metaOut.cls.expect(7.U(8.W))
    }
  }

  it should "③ 更新已有项：旧动作不再出现，新参数生效（set_port 双参数）" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(3.U(8.W))

      writeEntry(c, 0, entry(1, 0x0800, argsPort(0x4321, 0x56)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(7.U(8.W)) // 旧动作（set_cls）不再出现 → 透传
      c.io.metaOut.normPort.expect(0x4321.U(16.W))
      c.io.metaOut.tag.expect(0x56.U(8.W))
    }
  }

  it should "④ 删除：写 valid=0 的表项后该 key 回 miss" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(3.U(8.W))

      writeEntry(c, 0, entry(0, 0x0800, argsCls(3), valid = false))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(7.U(8.W))
      // 删除后重新写入可恢复
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(3.U(8.W))
    }
  }

  it should "⑤ 写拍当拍发起查找：结果为旧值或新值之一（无撕裂）" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(3.U(8.W))

      // 写口与查找同拍：写不消费 valid、查找不感知 we，两条路径仅共享寄存器阵列
      c.io.tbl_rt_table_we.poke(true.B)
      c.io.tbl_rt_table_waddr.poke(0.U(3.W))
      c.io.tbl_rt_table_wdata.poke(entry(0, 0x0800, argsCls(9)).U(EntryW.W))
      val during = c.io.metaOut.cls.peek().litValue
      assert(during == 3 || during == 9, s"写拍查找必须看到旧值或新值之一（实际 $during）")

      c.clock.step(1)
      c.io.tbl_rt_table_we.poke(false.B)
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(9.U(8.W)) // 下一拍起看到新值
    }
  }

  it should "⑥ 越界写（waddr ≥ size）被忽略，表内容不变" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(5)))
      writeEntry(c, 6, entry(0, 0x0800, argsCls(0xee))) // size=6：addr 6/7 越界
      writeEntry(c, 7, entry(1, 0x0800, argsPort(0xbeef, 0x11)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(5.U(8.W))
      c.io.metaOut.normPort.expect(0x1234.U(16.W))
      c.io.metaOut.tag.expect(0xab.U(8.W))
    }
  }

  it should "⑦ 静态表与运行时表共存：各自独立命中" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      pokeIn(c, 0x0800, srcAddr = BigInt(0x02))
      c.io.metaOut.stat.expect(5.U(8.W)) // 静态表命中（srcAddr=0x02 → set_stat(5)）
      c.io.metaOut.cls.expect(3.U(8.W)) // 运行时表命中

      pokeIn(c, 0x0800, srcAddr = BigInt(0x99))
      c.io.metaOut.stat.expect(1.U(8.W)) // 静态表 miss → 透传
      c.io.metaOut.cls.expect(3.U(8.W))
    }
  }

  it should "⑧ 非法 actionId（未定义编号）等同 default：所有字段透传" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(3, 0x0800, argsCls(3))) // act=3 未定义（仅 0/1/2）
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(7.U(8.W))
      c.io.metaOut.normPort.expect(0x1234.U(16.W))
      c.io.metaOut.tag.expect(0xab.U(8.W))
    }
  }

  it should "重复 key 多命中：低地址优先（PriorityMux，同静态表声明序）" in {
    test(new Demo7RuntimeTableIngress) { c =>
      writeEntry(c, 0, entry(0, 0x0800, argsCls(3)))
      writeEntry(c, 2, entry(0, 0x0800, argsCls(9)))
      pokeIn(c, 0x0800)
      c.io.metaOut.cls.expect(3.U(8.W))
    }
  }
}
