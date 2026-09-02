"""
OSA architecture validation suite.

Runs the cycle-accurate model (osa_model.py) against the claims of
docs/OSA.md §6.1/§6.3:

  T1  steady state, in = out = 1.6 Tbps        -> write 20/c, read 20/c, bounded backlog/latency
  T2  writes at line rate + read demand 24     -> does out-of-order execution reach 24/c?
  T3  drain mode (writes stopped)              -> read 24/c (full 2x96B beat)
  T4  worst-case alignment (delta = 0)         -> 1-cycle stall then self-correction
  T5  read latency bound                       -> bounded queueing latency
  T6  burst absorption + PFC backpressure loop -> no loss, XOFF absorption, XON recovery
  T7  random long-run traffic                  -> conservation, bounded backlog, no loss
  T8  theory cross-check (W, R) matrix         -> model vs analytic expectations

Run:  python3 tools/osa_sim/osa_tests.py
"""

import random
from osa_model import OSAConfig, OSASim

PASS = []


def report(name, ok, detail):
    tag = "PASS" if ok else "FAIL"
    PASS.append(ok)
    print(f"[{tag}] {name}")
    print(f"      {detail}")


# --------------------------------------------------------------------------
# T1: steady state, 1.6 Tbps in and out
# --------------------------------------------------------------------------
def t1_steady_state():
    sim = OSASim()
    sim.warmup(200)                       # build occupancy (4000 segs)
    cycles = 20000
    for _ in range(cycles):
        sim.step(20, 20)
    # measure the last 19000 cycles (skip the first 1000 alignment transient)
    wr_avg = sum(sim.wr_hist[-19000:]) / 19000.0
    rd_avg = sum(sim.exec_hist[-19000:]) / 19000.0
    ok = (abs(wr_avg - 20.0) < 0.01 and abs(rd_avg - 20.0) < 0.01)
    report("T1 steady state (in=out=1.6 Tbps)",
           ok,
           f"write avg={wr_avg:.3f}/c, read avg={rd_avg:.3f}/c, "
           f"backlog_max={sim.backlog_max}, lat_avg={sim.avg_latency:.2f}c, "
           f"lat_max={sim.lat_max}c")
    assert abs(wr_avg - 20.0) < 0.01
    assert abs(rd_avg - 20.0) < 0.01
    assert sim.backlog_max <= 48, "steady-state backlog should be bounded (~<=24 + transient)"
    assert sim.lat_max <= 8, "steady-state latency should be bounded"


# --------------------------------------------------------------------------
# T2: writes at line rate + read demand 24 (full 2x96B beat)
# --------------------------------------------------------------------------
def t2_write_full_read_peak():
    sim = OSASim()
    sim.warmup(500)                       # occupancy 10000, available 10000
    cycles = 2000                         # drains 4/c -> 8000 segs, still available
    for _ in range(cycles):
        sim.step(20, 24)
    rd_avg = sum(sim.exec_hist[-2000:]) / 2000.0
    gen_avg = sum(sim.gen_hist[-2000:]) / 2000.0
    # Out-of-order execution: with a well-populated read queue (banks spread
    # over all 44 banks), 24 free slots per cycle can all be filled even while
    # writes claim their 20 banks. Expect read ~24/c (drain mode), NOT the
    # conservative 20/c ceiling.
    ok = rd_avg >= 23.0
    report("T2 writes@line-rate + read demand 24",
           ok,
           f"read exec avg={rd_avg:.3f}/c (peak 24), gen avg={gen_avg:.3f}/c, "
           f"backlog_max={sim.backlog_max}, lat_avg={sim.avg_latency:.2f}c")
    assert rd_avg >= 23.0, "out-of-order execution should sustain ~24 reads/c with backlog"


# --------------------------------------------------------------------------
# T3: drain mode (writes stopped), read demand 24
# --------------------------------------------------------------------------
def t3_drain_mode():
    sim = OSASim()
    sim.warmup(500)                       # occupancy 10000 (drain 24/c x 400 = 9600 < 10000)
    cycles = 400
    for _ in range(cycles):
        sim.step(0, 24)
    rd_avg = sum(sim.exec_hist[-400:]) / 400.0
    ok = abs(rd_avg - 24.0) < 0.01
    report("T3 drain mode (write=0, read=24)",
           ok,
           f"read avg={rd_avg:.3f}/c (expect 24), backlog_max={sim.backlog_max}, "
           f"lat_avg={sim.avg_latency:.2f}c")
    assert abs(rd_avg - 24.0) < 0.01


# --------------------------------------------------------------------------
# T4: worst-case alignment (delta = 0) self-correction
# --------------------------------------------------------------------------
def t4_delta_self_correction():
    sim = OSASim()
    # warm up so that wr_ptr mod banks == 0 (write window == read window, delta=0).
    # step of +20/c over mod-44/64 space returns to 0 periodically.
    for _ in range(200):
        sim.step(20, 0)
    while sim.wr_ptr % sim.cfg.banks != 0:
        sim.step(20, 0)
    assert sim.wr_ptr % sim.cfg.banks == 0, \
        f"alignment setup failed: wr_ptr={sim.wr_ptr} mod {sim.cfg.banks}"
    # now write 20 + read 20 from the aligned state
    first_cycle_exec = sim.step(20, 20)          # expect ~0 (full conflict)
    execs = []
    for _ in range(20):
        execs.append(sim.step(20, 20))
    recovered = execs[2]                          # after a few cycles
    avg = sum(execs[5:]) / 15.0
    ok = (first_cycle_exec <= 4 and recovered >= 16 and avg >= 19.9)
    report("T4 worst-case delta=0 self-correction",
           ok,
           f"cycle0 exec={first_cycle_exec} (stall), execs={execs}, "
           f"avg(last15)={avg:.2f}/c, backlog_max={sim.backlog_max}")
    assert first_cycle_exec <= 4
    assert recovered >= 16
    assert avg >= 19.9


# --------------------------------------------------------------------------
# T5: read latency bound under steady state
# --------------------------------------------------------------------------
def t5_latency_bound():
    sim = OSASim()
    sim.warmup(200)
    for _ in range(20000):
        sim.step(20, 20)
    ok = sim.lat_max <= 8 and sim.avg_latency <= 3.0
    report("T5 read latency bound (steady state)",
           ok,
           f"lat_avg={sim.avg_latency:.2f}c, lat_max={sim.lat_max}c "
           f"(expected ~1-3c, bound 8)")
    assert sim.lat_max <= 8
    assert sim.avg_latency <= 3.0


# --------------------------------------------------------------------------
# T6: burst absorption + PFC backpressure loop (XOFF/XON), no loss
# --------------------------------------------------------------------------
def t6_bp_loop():
    cfg = OSAConfig()
    thr = 12000          # losslessThr (8B units), region 14080-ish
    hyst = 1024
    pfc_delay = 100      # cycles between BP assertion and MAC actually pausing
    sim = OSASim(cfg)
    bp = False
    pause_cnt = 0
    rd = 20
    overflow = 0
    for _ in range(30000):
        # backpressure state machine (per-port; global here for the model)
        if not bp and sim.occupancy > thr:
            bp = True
            pause_cnt = pfc_delay
        if bp:
            if pause_cnt > 0:
                pause_cnt -= 1
                wr = 20                  # MAC still sending (XOFF window)
            else:
                wr = 0                   # MAC paused
            if sim.occupancy < thr - hyst:
                bp = False
        else:
            wr = 20
        sim.step(wr, rd)
        if sim.occupancy > cfg.buffer_entries:
            overflow += 1
    # assertions: no buffer overflow, no read drop (model bounds generation),
    # occupancy recovered (XON), conservation holds
    cons = sim.wr_done == sim.rd_done + sim.occupancy
    ok = (overflow == 0 and cons and sim.occupancy < thr - hyst and
          sim.backlog_max <= cfg.read_queue_depth)
    report("T6 burst + PFC loop (XOFF/XON)",
           ok,
           f"overflow_cycles={overflow}, final_occupancy={sim.occupancy}, "
           f"backlog_max={sim.backlog_max}, wr_done={sim.wr_done}, "
           f"rd_done={sim.rd_done}, conservation={cons}")
    assert overflow == 0
    assert cons
    assert sim.occupancy < thr - hyst      # recovered after XON
    assert sim.backlog_max <= cfg.read_queue_depth


# --------------------------------------------------------------------------
# T7: random long-run traffic
# --------------------------------------------------------------------------
def t7_random_long_run():
    rng = random.Random(20260517)
    sim = OSASim()
    sim.warmup(100)
    cycles = 50000
    for _ in range(cycles):
        wr = rng.randint(0, 20)
        sim.step(wr, 20)
    rd_avg = sum(sim.exec_hist[-50000:]) / 50000.0
    wr_avg = sum(sim.wr_hist[-50000:]) / 50000.0
    cons = sim.wr_done == sim.rd_done + sim.occupancy
    ok = (cons and sim.backlog_max <= 64 and 0 <= sim.occupancy <= sim.cfg.buffer_entries
          and rd_avg <= 20.0 + 0.1)
    report("T7 random long-run traffic",
           ok,
           f"wr_avg={wr_avg:.2f}/c, rd_avg={rd_avg:.2f}/c, backlog_max={sim.backlog_max}, "
           f"occupancy={sim.occupancy}, conservation={cons}")
    assert cons
    assert sim.backlog_max <= 64
    assert 0 <= sim.occupancy <= sim.cfg.buffer_entries


# --------------------------------------------------------------------------
# T8: theory cross-check matrix (W, R) -> executed read rate
# --------------------------------------------------------------------------
def t8_theory_crosscheck():
    results = []
    # (W, R, warmup, cycles, expect_E, note)
    #   R=24 drains data at (R - W) seg/c -> warmup must provision enough
    #   segments so the measurement window never runs out of data.
    cases = [
        (20, 20, 200, 3000, 20.0, "steady 1.6T"),
        (20, 24, 500, 2000, 24.0, "write-full + read 24 (drain, backlog)"),
        (10, 24, 1500, 1500, 24.0, "half write + read 24"),
        (0, 24, 2000, 1200, 24.0, "drain mode"),
        (20, 12, 200, 3000, 12.0, "read below capacity"),
        (5, 24, 1800, 1200, 24.0, "light write + read 24"),
    ]
    all_ok = True
    for (w, r, wu, n, exp, note) in cases:
        sim = OSASim()
        sim.warmup(wu, n_wr=20, rd_demand=0)
        for _ in range(n):
            sim.step(w, r)
        avg = sum(sim.exec_hist[-n:]) / n
        ok = abs(avg - exp) < 0.6
        all_ok &= ok
        results.append(f"  W={w:2d} R={r:2d}: exec={avg:6.2f}/c (expect {exp:5.1f})  "
                       f"{'OK' if ok else 'MISMATCH'}  [{note}]")
    report("T8 theory cross-check (W,R) matrix",
           all_ok,
           "\n".join(results))
    assert all_ok


# --------------------------------------------------------------------------
# T9: per-read queueing latency bound (the "data on a bank being written"
#     question). Write window rotates +20 banks/cycle => no bank is written
#     two cycles in a row => a write-deferred read waits <= 1 cycle; with a
#     full slot table the total queueing delay stays <= 2 cycles.
# --------------------------------------------------------------------------
def t9_per_read_queueing_latency():
    results = []
    cases = [
        (20, 20, 200, 20000, 1, "steady 1.6T"),
        (20, 24, 500, 100000, 2, "write-full + read 24 (stress)"),
        (0, 24, 500, 400, 0, "drain mode (no writes)"),
    ]
    all_ok = True
    for (w, r, wu, n, exp_max, note) in cases:
        sim = OSASim()
        sim.warmup(wu)
        for _ in range(n):
            sim.step(w, r)
        got = sim.q_delay_max
        ok = got <= exp_max
        all_ok &= ok
        # distribution of queueing delays
        hist = sim.q_delay_hist
        dist = {}
        for d in hist:
            dist[d] = dist.get(d, 0) + 1
        total = len(hist)
        dist_str = ", ".join(f"{k}c:{100*v/total:.1f}%" for k, v in sorted(dist.items()))
        results.append(f"  W={w} R={r}: queueing_delay max={got} (bound {exp_max})  "
                       f"{'OK' if ok else 'EXCEEDED'}  [{note}]  dist={dist_str}")
    report("T9 per-read queueing latency bound",
           all_ok,
           "\n".join(results))
    assert all_ok


# --------------------------------------------------------------------------
# T10: adversarial bank hotspot (all reads funneled into one bank) -- must be
#      structurally impossible with sequential read generation; the model
#      demonstrates the delay pathology and the queue staying bounded.
# --------------------------------------------------------------------------
def t10_adversarial_bank_hotspot():
    from collections import deque
    # subclass: force every generated read onto a single bank
    class HotspotSim(OSASim):
        def step(self, n_wr, rd_demand, hot_bank=10):
            c = self.cycle
            self.cycle += 1
            n_wr = max(0, min(int(n_wr), self.cfg.wr_segs))
            wr_banks = set()
            if n_wr:
                base = self.wr_ptr % self.cfg.buffer_entries
                for i in range(n_wr):
                    wr_banks.add((base + i) % self.cfg.banks)
                self.wr_ptr = (self.wr_ptr + n_wr) % self.cfg.buffer_entries
            self.wr_done += n_wr
            avail = self.wr_done - self.rd_gen
            q_space = self.cfg.read_queue_depth - len(self.read_q)
            n_gen = max(0, min(int(rd_demand), self.cfg.rd_peak, avail, q_space))
            for _ in range(n_gen):
                a = (self.rd_gen_ptr % self.cfg.buffer_entries)
                a = a - (a % self.cfg.banks) + hot_bank   # force hot bank
                self.read_q.append((a % self.cfg.buffer_entries, c, 0))
            self.rd_gen += n_gen
            self.rd_gen_ptr = (self.rd_gen_ptr + n_gen) % self.cfg.buffer_entries
            free_slots = self.cfg.banks - len(wr_banks)
            executed = 0
            bank_taken = set()
            kept = deque()
            for a, g, d in self.read_q:
                b = a % self.cfg.banks
                if b in wr_banks or b in bank_taken or executed >= free_slots:
                    kept.append((a, g, d + 1))
                    continue
                bank_taken.add(b)
                executed += 1
                self.rd_done += 1
                self.q_delay_hist.append(d)
                if d > self.q_delay_max:
                    self.q_delay_max = d
            self.read_q = kept
            if len(self.read_q) > self.backlog_max:
                self.backlog_max = len(self.read_q)
            self.exec_hist.append(executed)
            return executed

    sim = HotspotSim()
    sim.warmup(500)
    for _ in range(20000):
        sim.step(20, 24)
    # Expectation: sequential reads are what the design relies on; a hotspot is
    # an input anomaly that the model exposes (large queueing delay) while the
    # queue itself stays bounded (generation is flow-controlled).
    ok = sim.backlog_max <= sim.cfg.read_queue_depth
    report("T10 adversarial single-bank hotspot (input anomaly)",
           ok,
           f"queueing_delay_max={sim.q_delay_max} cycles (pathological input), "
           f"backlog_max={sim.backlog_max} <= queue depth "
           f"({sim.cfg.read_queue_depth}); normal sequential reads never form "
           f"a hotspot (T9: max 2 cycles)")
    assert ok


# --------------------------------------------------------------------------
# T11-T15: work-conserving egress with TWO loopback ports (each <= 300 Gbps).
# Loopback data lives in DEDICATED TP memories (8 banks x 32B = 256 B/c read
# port = 32 seg/c each), NOT in the main buffer.
# Scheduling: the OSA read is STRICT PRIORITY on the 2 x 96B egress (24 seg/c);
# loopback ports are WORK-CONSERVING -- they only transmit in the egress
# capacity left unused by the OSA read, each rate-limited to <= 300 Gbps
# (3.75 seg/c token bucket). The 300 Gbps cap is only reachable when the
# network read is below 1.32 Tbps (24 - 2*3.75 = 16.5 seg/c).
# Main buffer banks = 44 (20 W + 24 R peak, 2 x 96B egress).
# --------------------------------------------------------------------------
LOOP_INJ_300G = [4, 4, 4, 3]     # 15 segs / 4 cycles = 3.75 seg/c = 30 B/c
LOOP_RATES = (3.75, 3.75)        # 300 Gbps per port


def t11_osa_1p6t_loopback_remaining():
    # OSA at 1.6T (20 seg/c): egress leaves 24-20 = 4 seg/c for loopback
    # -> each port gets ~2 seg/c (160 Gbps), far below its 300 Gbps cap.
    cfg = OSAConfig(loop_rates=LOOP_RATES)
    sim = OSASim(cfg)
    sim.warmup(5000)                          # occupancy 100000
    cycles = 100000
    for cyc in range(cycles):
        sim.step(20, 20, loop_in=LOOP_INJ_300G[cyc % 4],
                 loop_in1=LOOP_INJ_300G[cyc % 4])
    osa_avg = sum(sim.exec_hist[-cycles:]) / cycles
    l0 = sum(sim.loop_out_hist[0][-cycles:]) / cycles
    l1 = sum(sim.loop_out_hist[1][-cycles:]) / cycles
    egress_util = (sum(sim.exec_hist[-cycles:]) + sum(sim.loop_out_hist[0][-cycles:])
                   + sum(sim.loop_out_hist[1][-cycles:])) / cycles
    ok = (abs(osa_avg - 20.0) < 0.05 and abs((l0 + l1) - 4.0) < 0.15
          and l0 <= 3.76 and l1 <= 3.76 and egress_util <= 24.01)
    report("T11 OSA 1.6T + loopbacks use remaining egress",
           ok,
           f"OSA read={osa_avg:.2f} seg/c (20 = 1.6T), loop0={l0:.2f} seg/c = "
           f"{l0*80:.0f} Gbps, loop1={l1:.2f} seg/c = {l1*80:.0f} Gbps "
           f"(sum ~4 = remaining; caps 300 Gbps NOT reached), egress={egress_util:.2f} <= 24")
    assert abs(osa_avg - 20.0) < 0.05
    assert abs((l0 + l1) - 4.0) < 0.15
    assert l0 <= 3.76 and l1 <= 3.76
    assert egress_util <= 24.01


def t12_loopback_300g_when_network_idle():
    # Network idle (OSA reads 0): loopbacks reach their 300 Gbps caps
    cfg = OSAConfig(loop_rates=LOOP_RATES)
    sim = OSASim(cfg)
    cycles = 100000
    for cyc in range(cycles):
        sim.step(0, 0, loop_in=LOOP_INJ_300G[cyc % 4],
                 loop_in1=LOOP_INJ_300G[cyc % 4])
    l0 = sum(sim.loop_out_hist[0][-cycles:]) / cycles
    l1 = sum(sim.loop_out_hist[1][-cycles:]) / cycles
    ok = abs(l0 - 3.75) < 0.01 and abs(l1 - 3.75) < 0.01
    report("T12 loopbacks reach 300 Gbps each when network idle",
           ok,
           f"loop0={l0:.3f} seg/c = {l0*80:.0f} Gbps, loop1={l1:.3f} seg/c = "
           f"{l1*80:.0f} Gbps (expect 3.75 seg/c = 300 Gbps each, caps reached)")
    assert abs(l0 - 3.75) < 0.01 and abs(l1 - 3.75) < 0.01


def t13_osa_full_squeezes_loopback():
    # OSA drains at full 24 seg/c: no egress left -> loopbacks starve
    cfg = OSAConfig(loop_rates=LOOP_RATES)
    sim = OSASim(cfg)
    sim.warmup(5000)                          # occupancy 100000 <= buffer, >= 24 x 4000
    cycles = 4000
    for cyc in range(cycles):
        sim.step(0, 24, loop_in=LOOP_INJ_300G[cyc % 4],
                 loop_in1=LOOP_INJ_300G[cyc % 4])
    osa_avg = sum(sim.exec_hist[-cycles:]) / cycles
    l0 = sum(sim.loop_out_hist[0][-cycles:]) / cycles
    l1 = sum(sim.loop_out_hist[1][-cycles:]) / cycles
    ok = (abs(osa_avg - 24.0) < 0.05 and l0 < 0.01 and l1 < 0.01)
    report("T13 OSA full-rate read squeezes loopbacks to zero",
           ok,
           f"OSA read={osa_avg:.2f} seg/c (24 = full egress), loop0={l0:.3f}, "
           f"loop1={l1:.3f} seg/c (expect ~0: no remaining egress)")
    assert abs(osa_avg - 24.0) < 0.05
    assert l0 < 0.01 and l1 < 0.01


def t14_osa_1p32t_loopback_300g():
    # OSA at 1.32 Tbps (16.5 seg/c): egress leaves 24-16.5 = 7.5 seg/c
    # -> both loopbacks reach their 300 Gbps caps (3.75 each).
    # write = read = 16.5 seg/c steady state (buffer stays balanced)
    cfg = OSAConfig(loop_rates=LOOP_RATES)
    sim = OSASim(cfg)
    sim.warmup(5000)
    cycles = 100000
    for cyc in range(cycles):
        wr = 17 if cyc % 2 == 0 else 16       # 16.5 seg/c average
        sim.step(wr, wr, loop_in=LOOP_INJ_300G[cyc % 4],
                 loop_in1=LOOP_INJ_300G[cyc % 4])
    osa_avg = sum(sim.exec_hist[-cycles:]) / cycles
    l0 = sum(sim.loop_out_hist[0][-cycles:]) / cycles
    l1 = sum(sim.loop_out_hist[1][-cycles:]) / cycles
    ok = (abs(osa_avg - 16.5) < 0.05 and abs(l0 - 3.75) < 0.02 and abs(l1 - 3.75) < 0.02)
    report("T14 OSA 1.32T + loopbacks reach 300 Gbps each",
           ok,
           f"OSA read={osa_avg:.2f} seg/c (16.5 = 1.32T), loop0={l0:.3f} seg/c = "
           f"{l0*80:.0f} Gbps, loop1={l1:.3f} seg/c = {l1*80:.0f} Gbps "
           f"(expect 3.75 each = 300 Gbps caps reached)")
    assert abs(osa_avg - 16.5) < 0.05
    assert abs(l0 - 3.75) < 0.02 and abs(l1 - 3.75) < 0.02


def t15_random_with_two_loopbacks():
    rng = random.Random(300)
    cfg = OSAConfig(loop_rates=LOOP_RATES)
    sim = OSASim(cfg)
    sim.warmup(200)
    cycles = 100000
    for cyc in range(cycles):
        sim.step(rng.randint(0, 20), 20, loop_in=LOOP_INJ_300G[cyc % 4],
                 loop_in1=LOOP_INJ_300G[cyc % 4])
    cons = sim.wr_done == sim.rd_done + sim.occupancy
    egress_util = (sum(sim.exec_hist) + sum(sim.loop_out_hist[0])
                   + sum(sim.loop_out_hist[1])) / cycles
    ok = (cons and egress_util <= 24.01 and 0 <= sim.occupancy <= sim.cfg.buffer_entries)
    report("T15 random traffic + two work-conserving loopbacks",
           ok,
           f"conservation={cons}, egress util={egress_util:.2f} seg/c <= 24, "
           f"occupancy={sim.occupancy}")
    assert cons
    assert egress_util <= 24.01
    assert 0 <= sim.occupancy <= sim.cfg.buffer_entries


def main():
    print("=" * 78)
    print("OSA architecture validation  (docs/OSA.md §6.1/§6.3)")
    print("=" * 78)
    random.seed(20260517)
    t1_steady_state()
    t2_write_full_read_peak()
    t3_drain_mode()
    t4_delta_self_correction()
    t5_latency_bound()
    t6_bp_loop()
    t7_random_long_run()
    t8_theory_crosscheck()
    t9_per_read_queueing_latency()
    t10_adversarial_bank_hotspot()
    t11_osa_1p6t_loopback_remaining()
    t12_loopback_300g_when_network_idle()
    t13_osa_full_squeezes_loopback()
    t14_osa_1p32t_loopback_300g()
    t15_random_with_two_loopbacks()
    print("=" * 78)
    npass = sum(PASS)
    print(f"RESULT: {npass}/{len(PASS)} passed")
    if npass == len(PASS):
        print("ALL TESTS PASSED - architecture claims of §6.1/§6.3 are reproduced by the model")
    else:
        print("SOME TESTS FAILED - investigate before proceeding")
    return 0 if npass == len(PASS) else 1


if __name__ == "__main__":
    import sys
    sys.exit(main())
