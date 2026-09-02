# OSA Performance Model & Validation Suite

Cycle-accurate model and test environment for the OSA buffer architecture
described in [`docs/OSA.md`](../../docs/OSA.md) (§6.1 buffer organization,
§6.2 slot arbitration, §6.3 arbitration priority & performance guarantees).

## Quick start

```bash
python3 tools/osa_sim/osa_tests.py      # run the full validation suite
python3 - <<'EOF'                        # quick interactive probe
from osa_sim.osa_model import OSASim
sim = OSASim()
sim.warmup(200)                          # 200 cycles write-only
sim.step(20, 20)                         # one cycle: 20 writes, 20 reads
print(sim.stats())
EOF
```

Requires only the Python standard library (Python ≥ 3.8).

## What the model simulates

The model (`osa_model.py`, class `OSASim`) is a **cycle-accurate model of the
bank-arbitration stage** of the OSA:

- **44 banks × 8B single-port SRAM** at the logic clock → 44 access
  slots/cycle.
- **Writes are hard real-time**: every cycle the requested `n_wr ≤ 20`
  consecutive segments are written at `wr_ptr`; the position-interleaved
  mapping `bank = addr mod 44` guarantees 20 distinct banks (writes never
  conflict with each other, and never lose).
- **Reads are elastic, executed out of order**: up to `rd_demand` new read
  requests are generated (bounded by unread data and the read-queue depth);
  each cycle the queue is scanned and every request whose bank is neither
  written this cycle nor already serving a read is executed; the rest are
  deferred (time-multiplexed sharing of bank bandwidth). This matches the RTL
  design where a ReorderQueue reassembles out-of-order responses (§3.10).
- **Statistics**: executed read rate, queue backlog, queueing latency,
  conflict-defer count, buffer occupancy.

Parameters mirror the design: `banks=44`, `wr_segs=20`, `rd_peak=24`
(2 × 96B beat), `read_queue_depth=64`, `mem_latency=1`,
`buffer_entries=112640` (880 KB / 8 B).

## Test cases vs. design claims (docs/OSA.md §6.1/§6.3)

| Test | Scenario | Verifies |
|------|----------|----------|
| T1 | steady state, in = out = 1.6 Tbps | write 20/c + read 20/c sustained, bounded backlog & latency |
| T2 | writes at line rate + read demand 24 | **read 24/c sustained even with W=20** (out-of-order execution) |
| T3 | drain mode (writes stopped) | read 24/c = full 2 × 96B beat |
| T4 | worst-case alignment δ = 0 | 1-cycle stall then self-correction to 20/c |
| T5 | latency bound | read queueing latency bounded (~1.8 c avg, 2 c max) |
| T6 | burst + PFC backpressure loop (XOFF/XON) | no overflow, no loss, conservation, XON recovery |
| T7 | random long-run traffic | conservation, bounded backlog, bounded occupancy |
| T8 | (W, R) throughput matrix | model matches analytic expectation for every combination |
| T9 | per-read queueing latency | **write-deferred read waits ≤ 1 cycle** (steady state) / ≤ 2 cycles (write-full + read-24 stress) — see §6.3 |
| T10 | adversarial single-bank hotspot | pathological input is exposed (large delay) while the queue stays bounded; impossible with sequential reads |
| T11 | work-conserving: OSA 1.6T + loopbacks | OSA 20.00 seg/c (1.6T), loopbacks share the 4 seg/c leftover (≈160 Gbps each, caps not reached) |
| T12 | network idle | both loopbacks reach **300 Gbps** (3.75 seg/c each, caps) |
| T13 | OSA full read (24 seg/c) | loopbacks starved to 0 (no leftover) |
| T14 | OSA 1.32T (16.5 seg/c) | leftover 7.5 seg/c → both loopbacks reach **300 Gbps** |
| T15 | random traffic + loopbacks | conservation, egress ≤ 24 seg/c |

## Validation outcome (2026-05-17)

**15/15 tests pass.** (work-conserving dual loopback, dedicated TP memory, 44 banks, 2 × 96B egress) The measured numbers reproduce and refine the analytic
claims:

```
T1  write avg=20.000/c  read avg=20.000/c  backlog_max=16  lat_avg=1.80c  lat_max=2c
T2  read exec avg=23.990/c  (demand 24, writes at line rate)   lat_avg=1.83c
T3  read avg=24.000/c  (drain mode)
T4  cycle0 exec=0 (δ=0 stall) → 20/c from cycle 1, avg(last15)=20.00/c
T5  lat_avg=1.80c, lat_max=2c
T6  overflow_cycles=0, conservation=True, final_occupancy recovered
T7  wr_avg=10.03/c, rd_avg=10.07/c, conservation=True, occupancy bounded
T8  W20R20→20.0  W20R24→24.0  W10R24→24.0  W0R24→24.0  W20R12→12.0  W5R24→24.0
T9  queueing delay: steady max=1c (80% of reads wait 1c); write-full+read24 max=2c (98.6% wait 1c); drain 0c
T10 hotspot (all reads → one bank): delay_max=117c, backlog bounded at 64 — input anomaly, not arbitration defect
T11 work-conserving: OSA 20.00 seg/c (1.6T) + loopbacks 4 seg/c total (≈160G each)
T12 network idle: loopbacks 3.750 seg/c = 300 Gbps each (caps reached)
T13 OSA full read: 24.00 seg/c, loopbacks 0.000 (no leftover)
T14 OSA 1.32T (16.5): loopbacks 3.750 seg/c = 300 Gbps each
T15 random + loopbacks: conservation, egress ≤ 24 seg/c
```

### Egress scheduling: work-conserving loopback (features 10/11, docs/OSA.md §3.14/§3.15)

The read-side egress is the **2 × 96B bus (24 seg/c, 1.92 Tbps)**. The OSA
read has **strict priority** on all 24 segments/cycle; the **two loopback
ports** are **work-conserving** — they transmit only in the egress capacity
left by the OSA read, each rate-limited to **300 Gbps** by a token bucket
(3.75 seg/c). Loopback data lives in **dedicated TP memories** (8 banks ×
32B dual-port per port — 256 B/c read port), separate from the main buffer.

- A loopback port reaches its 300 Gbps cap **only when the network read is
  below 1.32 Tbps** (leftover ≥ 7.5 seg/c). At the full 1.6 Tbps read the
  leftover is 4 seg/c (320 Gbps) → ≈ 160 Gbps per port — per the requirement
  that loopback bandwidth is available only when the network is not
  saturated.
- Main buffer: 44 banks (20 W + 24 R peak), 2 × 96B egress; each 96B unit
  belongs to exactly one packet (unit-aligned packets, no packing).
- Verified (T11–T15): OSA 1.6T + loopbacks take the 4 seg/c leftover; network
  idle → loopbacks reach 300 Gbps each; OSA full 24 seg/c → loopbacks 0; OSA
  1.32T → loopbacks 300 Gbps each; random traffic conserves.

### Per-read latency guarantee (the "data on a bank being written" question)

With write-priority arbitration, a read whose bank is being written waits
**at most 1 cycle** — provably: the 20-bank write window shifts by 20 banks
every cycle, so the windows of consecutive cycles `[w, w+20)` and
`[w+20, w+40)` are disjoint on the 44-bank ring and **no bank is written in
two consecutive cycles**. The deferred read finds its bank free on the very
next cycle. Total queueing delay stays ≤ 2 cycles even under write-full +
read-24 stress (the extra cycle is slot contention, `bank_taken`), verified
over 10^5 cycles / 2×10^6 reads (T9: 99.55% of reads queue ≤ 1 cycle).
This is a *slot* conflict, not a data conflict: the write targets a different
row of the same bank, so the wanted data is unaffected — it only waits for
the bank port.

The one caveat (T10): if a pathological input funnels *all* reads onto a
single bank, that bank's 1-read/cycle service rate dominates and queueing
delay grows (117 cycles in the model). This cannot occur in normal operation
because read addresses are generated by sequential packet scans
(`bufBase + 8 + segIdx`) — each cycle's ≤ 24 requests land on distinct
banks — so no hotspot ever forms.

### How to extend

- The model abstracts away per-port / per-packet logic (admission, PFC
  priorities, cell formatting). To validate those layers, extend `OSASim` with
  per-port occupancy + threshold logic (T6 already sketches the global PFC
  loop) or drive it from a packet-level traffic generator.
- The same model can serve as a **golden reference** for the Chisel RTL once
  the OSA is implemented (`src/main/scala/FPP/OSA/`): compare per-cycle
  executed-read counts and backlog against `OSASim` in co-simulation.
