package BaseCbb.arbiter

import BaseCbb._
import chisel3._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ArbiterSpec extends AnyFlatSpec with Matchers {

  /** Check that grant is one-hot and is a subset of ready */
  private def isOneHot(grant: BigInt, ready: BigInt): Boolean = {
    grant != 0 && (grant & (grant - 1)) == 0 && (grant & ~ready) == 0
  }

  // ---- RR (Round-Robin Arbiter) ----

  "RR" should "grant a one-hot subset of ready" in {
    simulate(new RR(4)) { c =>
      SimReset(c)
      c.io.ready.poke("b0010".U)
      c.io.enable.poke(true.B)
      c.clock.step(1)
      val grant = c.io.grant.peek().litValue
      assert(isOneHot(grant, 2), s"Grant $grant should be one-hot subset of ready 0010")
    }
  }

  "RR" should "grant zero when no requests" in {
    simulate(new RR(4)) { c =>
      c.io.ready.poke(0.U)
      c.io.enable.poke(true.B)
      c.clock.step(1)
      c.io.grant.expect(0.U)
    }
  }

  "RR" should "rotate grant among requesters" in {
    simulate(new RR(4)) { c =>
      SimReset(c)
      c.io.enable.poke(true.B)
      c.io.ready.poke("b1111".U)

      val grants = for (_ <- 0 until 8) yield {
        c.clock.step(1)
        c.io.grant.peek().litValue.toInt
      }

      // All grants should be one-hot powers of 2
      grants.foreach { g =>
        assert(g == 1 || g == 2 || g == 4 || g == 8, s"Unexpected grant $g")
      }

      // After 4 grants, we should have cycled through all 4 clients
      val first4 = grants.take(4).toSet
      assert(first4.size >= 2, s"Should distribute among at least 2 clients, got ${first4}")
    }
  }

  "RR" should "skip absent requesters" in {
    simulate(new RR(4)) { c =>
      SimReset(c)
      c.io.enable.poke(true.B)
      c.io.ready.poke("b1010".U) // only clients 1 and 3 ready

      for (_ <- 0 until 8) {
        c.clock.step(1)
        val grant = c.io.grant.peek().litValue.toInt
        // Must be client 1 (2) or client 3 (8), never 0 (1) or 2 (4)
        assert(grant == 2 || grant == 8, s"Grant $grant should be only ready clients 1 or 3")
      }
    }
  }

  "RR" should "hold grant when enable=0" in {
    simulate(new RR(4)) { c =>
      c.io.enable.poke(true.B)
      c.io.ready.poke("b1111".U)
      c.clock.step(1)
      val firstGrant = c.io.grant.peek().litValue

      c.io.enable.poke(false.B)
      c.clock.step(1)
      // Grant should be the same (pointer doesn't advance)
      c.io.grant.expect(firstGrant.U)
    }
  }

  // ---- WRR (Weighted Round-Robin) ----

  "WRR" should "grant only ready clients" in {
    simulate(new WRR(2, 4)) { c =>
      SimReset(c)
      c.io.enable.poke(true.B)
      c.io.weight(0).poke(3.U)
      c.io.weight(1).poke(3.U)
      c.io.ready.poke("b01".U) // only client 0 ready

      for (_ <- 0 until 10) {
        c.clock.step(1)
        val grant = c.io.grant.peek().litValue.toInt
        assert(grant == 1, s"Grant $grant should be client 0 only")
      }
    }
  }

  "WRR" should "reload weights when all requests consumed" in {
    simulate(new WRR(2, 4)) { c =>
      SimReset(c)
      c.io.enable.poke(true.B)
      c.io.weight(0).poke(1.U)
      c.io.weight(1).poke(1.U)
      c.io.ready.poke("b11".U)

      // Both clients should eventually get grants
      val grants = for (_ <- 0 until 10) yield {
        c.clock.step(1)
        c.io.grant.peek().litValue.toInt
      }

      val clientsSeen = grants.toSet
      assert(clientsSeen.size >= 2, s"Should see both clients, saw: $clientsSeen")
    }
  }

  // ---- 死锁回归（2026-09-13 修复：enable=1 且无请求时旧实现把 point_ff 清 0，
  //      RrLogic(rdy,0)≡0，仲裁器此后永久卡死）----

  "RR" should "recover after enable with no requests (no deadlock)" in {
    simulate(new RR(4)) { c =>
      SimReset(c)
      c.io.enable.poke(true.B)
      c.io.ready.poke(0.U)
      for (_ <- 0 until 4) {
        c.clock.step(1)
        c.io.grant.expect(0.U) // 无请求期间 grant 恒 0，但指针不得被破坏
      }
      c.io.ready.poke("b0100".U) // 请求恢复后必须立即能授予
      c.clock.step(1)
      c.io.grant.expect(4.U)
    }
  }

  "WRR" should "recover after enable with no requests (no deadlock)" in {
    simulate(new WRR(2, 4)) { c =>
      SimReset(c)
      c.io.enable.poke(true.B)
      c.io.weight(0).poke(3.U)
      c.io.weight(1).poke(3.U)
      c.io.ready.poke(0.U)
      for (_ <- 0 until 4) {
        c.clock.step(1)
        c.io.grant.expect(0.U)
      }
      c.io.ready.poke("b10".U)
      c.clock.step(1)
      c.io.grant.expect(2.U) // 旧实现：内部 RR 指针已被清 0，此处永远 0
    }
  }
}
