"""
OSA cycle-accurate performance model.

Implements the 44-bank single-port SRAM with **write-priority slot
arbitration** described in docs/OSA.md (§6.1, §6.2, §6.3):

  - 44 banks x 8B at the logic clock  =>  44 access slots per cycle
  - 20 slots claimed by writes (hard real-time; position-interleaved mapping
    `bank = addr mod 44` makes the 20 writes hit 20 distinct banks, so writes
    never conflict with each other)
  - remaining slots serve reads (elastic); a read whose bank is being written
    in the same cycle is **queued** and retried in a later cycle's free slot
    (time-multiplexed sharing of bank bandwidth)
  - reads may execute **out of order** (skipping banks being written), then
    get reassembled by a ReorderQueue at the interface -- the model only
    tracks throughput / backlog / latency of the bank-arbitration stage

Model semantics per cycle (`step(n_wr, rd_demand)`):
  1. writes: `n_wr` consecutive segments at `wr_ptr`; always executed.
  2. read generation: up to `rd_demand` new read requests (bounded by
     available unread data and by the read-queue depth) at `rd_gen_ptr`.
  3. arbitration: scan the read queue (FIFO order), execute every request
     whose bank is not being written this cycle and whose bank has not already
     served a read this cycle; defer the rest.

All claims of docs/OSA.md §6.3 (sustained throughput, bounded backlog and
latency, delta self-correction, peak behaviour) can be checked against this
model by tools/osa_sim/osa_tests.py.
"""

from collections import deque


class OSAConfig:
    def __init__(self,
                 banks: int = 44,        # 20 write + <=24 read peak (2 x 96B egress)
                 wr_segs: int = 20,      # input segments per cycle (1.6 Tbps @1.25 GHz)
                 rd_peak: int = 24,      # max read segments per cycle (2 x 96B beat)
                 read_queue_depth: int = 64,
                 mem_latency: int = 1,   # SRAM read latency in cycles
                 buffer_entries: int = 112640,  # 880 KB / 8 B
                 loop_rates: tuple = None,      # None = no loopback;
                                                # (rate0, rate1) in seg/cycle, e.g.
                                                # (3.75, 3.75) = 300 Gbps each @1.25 GHz.
                                                # Work-conserving: OSA reads first,
                                                # loopback ports use the remaining
                                                # egress, each rate-limited by its
                                                # token bucket (<= 300 Gbps).
                 loop_peak: int = 32):         # loopback TP memory read port:
                                                # 8 banks x 32B = 256 B/c = 32 seg/c
        self.banks = banks
        self.wr_segs = wr_segs
        self.rd_peak = rd_peak
        self.read_queue_depth = read_queue_depth
        self.mem_latency = mem_latency
        self.buffer_entries = buffer_entries
        self.loop_rates = loop_rates          # (rate0, rate1) seg/cycle or None
        self.loop_peak = loop_peak

    @property
    def egress_frame(self):
        # work-conserving scheduling has no fixed TDM frame
        return None

    @property
    def loop_slots(self):
        return 0

    @property
    def loop0_slots(self):
        return 0

    @property
    def loop1_slots(self):
        return 0

    @property
    def loop_bandwidth_bytes_per_cycle(self):
        """loopback bandwidth in B/cycle if fully satisfied (rate caps)."""
        if self.loop_rates is None:
            return 0.0
        return (self.loop_rates[0] + self.loop_rates[1]) * 8.0

    def __repr__(self):
        return (f"OSAConfig(banks={self.banks}, wr_segs={self.wr_segs}, "
                f"rd_peak={self.rd_peak}, read_queue_depth={self.read_queue_depth}, "
                f"mem_latency={self.mem_latency}, loop_rates={self.loop_rates}, "
                f"loop_peak={self.loop_peak})")


class OSASim:
    """Cycle-accurate model of the OSA bank-arbitration stage."""

    def __init__(self, cfg: OSAConfig = None):
        self.cfg = cfg or OSAConfig()
        # address pointers (absolute 8B-entry indices, wrap over buffer)
        self.wr_ptr = 0        # next write address
        self.rd_gen_ptr = 0    # next read-request address (generation side)
        # counters
        self.wr_done = 0       # segments written
        self.rd_gen = 0        # read requests generated
        self.rd_done = 0       # read requests executed
        self.read_q = deque()  # pending reads: (addr, gen_cycle, defer_count)
        self.cycle = 0
        # statistics
        self.lat_sum = 0.0     # read latency: queueing + mem_latency (cycles)
        self.lat_max = 0
        self.backlog_max = 0
        self.conflict_defer = 0  # requests deferred because bank was written
        self.q_delay_hist = []   # per-read queueing delay (cycles, excl. mem_latency)
        self.q_delay_max = 0
        self.exec_hist = []      # executed reads per cycle
        self.gen_hist = []       # generated reads per cycle
        self.wr_hist = []        # written segments per cycle
        # egress / loopback state (2 loopback ports, work-conserving)
        self.loop_q0 = deque()   # loopback port 0 segments
        self.loop_q1 = deque()   # loopback port 1 segments
        self.loop_in_total = [0, 0]
        self.loop_out_hist = [[], []]   # served segments per cycle per port
        self.loop_backlog_max = [0, 0]
        # token buckets: each loopback port is rate-limited (<= 300 Gbps =
        # 3.75 seg/c default); tokens accumulate at the rate, capped at the
        # bucket depth; loopback only transmits when the OSA read leaves
        # egress capacity (strict priority, work-conserving).
        self.loop_token = [0.0, 0.0]
        self.loop_token_cap = 24.0
        self.slot_hist = []      # "OSA" (all cycles: OSA is always preferred)

    def _pop_loop(self, idx, n):
        q = self.loop_q0 if idx == 0 else self.loop_q1
        n = min(int(n), len(q))
        for _ in range(n):
            q.popleft()
        return n

    @property
    def loop_backlog(self):
        return len(self.loop_q0) + len(self.loop_q1)

    # ---- derived state ---------------------------------------------------
    @property
    def backlog(self):
        return len(self.read_q)

    @property
    def occupancy(self):
        """segments written but not yet read (executed)."""
        return self.wr_done - self.rd_done

    @property
    def available(self):
        """segments written but not yet issued as read requests."""
        return self.wr_done - self.rd_gen

    @property
    def avg_latency(self):
        if self.rd_done == 0:
            return 0.0
        return self.lat_sum / self.rd_done

    # ---- one cycle -------------------------------------------------------
    def step(self, n_wr, rd_demand, loop_in=0, loop_in1=0):
        c = self.cycle
        self.cycle += 1
        self.slot_hist.append("OSA")   # OSA read is always the preferred source

        # loopback traffic injection (two ports, each <= its configured rate)
        if loop_in > 0:
            for _ in range(int(loop_in)):
                self.loop_q0.append(c)
            self.loop_in_total[0] += int(loop_in)
        if loop_in1 > 0:
            for _ in range(int(loop_in1)):
                self.loop_q1.append(c)
            self.loop_in_total[1] += int(loop_in1)

        # ---- 1. writes: hard real-time, always executed ------------------
        n_wr = max(0, min(int(n_wr), self.cfg.wr_segs))
        wr_banks = set()
        if n_wr:
            base = self.wr_ptr % self.cfg.buffer_entries
            for i in range(n_wr):
                wr_banks.add((base + i) % self.cfg.banks)
            self.wr_ptr = (self.wr_ptr + n_wr) % self.cfg.buffer_entries
        self.wr_done += n_wr
        self.wr_hist.append(n_wr)

        # ---- 2. read-request generation (bounded) ------------------------
        avail = self.wr_done - self.rd_gen
        q_space = self.cfg.read_queue_depth - len(self.read_q)
        n_gen = max(0, min(int(rd_demand), self.cfg.rd_peak, avail, q_space))
        if n_gen:
            base = self.rd_gen_ptr % self.cfg.buffer_entries
            for i in range(n_gen):
                self.read_q.append(((base + i) % self.cfg.buffer_entries, c, 0))
            self.rd_gen += n_gen
            self.rd_gen_ptr = (self.rd_gen_ptr + n_gen) % self.cfg.buffer_entries
        self.gen_hist.append(n_gen)

        # ---- 3. arbitration: OSA reads first (strict priority) -----------
        # Out-of-order execution: scan the queue and take every request whose
        # bank is free (not written this cycle, not already read this cycle).
        free_slots = self.cfg.banks - len(wr_banks)
        executed = 0
        bank_taken = set()
        kept = deque()
        deferred = 0
        for a, g, d in self.read_q:
            b = a % self.cfg.banks
            if b in wr_banks:
                deferred += 1
                kept.append((a, g, d + 1))
                continue
            if b in bank_taken:
                kept.append((a, g, d + 1))
                continue
            if executed >= free_slots or executed >= self.cfg.rd_peak:
                kept.append((a, g, d + 1))
                continue
            bank_taken.add(b)
            executed += 1
            self.rd_done += 1
            lat = c - g + self.cfg.mem_latency
            self.lat_sum += lat
            if lat > self.lat_max:
                self.lat_max = lat
            self.q_delay_hist.append(d)
            if d > self.q_delay_max:
                self.q_delay_max = d
        self.read_q = kept
        if len(self.read_q) > self.backlog_max:
            self.backlog_max = len(self.read_q)
        self.conflict_defer += deferred
        self.exec_hist.append(executed)

        # ---- 4. work-conserving loopback on the remaining egress --------
        # The egress carries 2 x 96B = 24 seg/c. OSA reads fill up to 24;
        # the leftover capacity is offered to the loopback ports (each
        # rate-limited by its token bucket so it never exceeds its cap, even
        # when the network is idle).
        loop_out0 = loop_out1 = 0
        if self.cfg.loop_rates is not None:
            remaining = self.cfg.rd_peak - executed
            # alternate the priority between the two loopback ports (WRR 1:1)
            order = (0, 1) if (c % 2 == 0) else (1, 0)
            for idx in order:
                if remaining <= 0:
                    break
                q = self.loop_q0 if idx == 0 else self.loop_q1
                rate = self.cfg.loop_rates[idx]
                # token bucket: accumulate rate seg/c, cap bucket
                self.loop_token[idx] = min(self.loop_token[idx] + rate,
                                           self.loop_token_cap)
                allowed = min(int(self.loop_token[idx]), len(q), remaining,
                              self.cfg.loop_peak)
                if allowed > 0:
                    self._pop_loop(idx, allowed)
                    self.loop_token[idx] -= allowed
                    remaining -= allowed
                    if idx == 0:
                        loop_out0 = allowed
                    else:
                        loop_out1 = allowed
        self.loop_out_hist[0].append(loop_out0)
        self.loop_out_hist[1].append(loop_out1)
        if len(self.loop_q0) > self.loop_backlog_max[0]:
            self.loop_backlog_max[0] = len(self.loop_q0)
        if len(self.loop_q1) > self.loop_backlog_max[1]:
            self.loop_backlog_max[1] = len(self.loop_q1)
        return executed

    # ---- batch helpers ---------------------------------------------------
    def warmup(self, cycles, n_wr=20, rd_demand=0):
        """Write-only warm-up to build buffer occupancy / read backlog."""
        for _ in range(cycles):
            self.step(n_wr, rd_demand)

    def run_steady(self, cycles, n_wr=20, rd_demand=20):
        """Run with constant write rate and read demand; returns (wr, rd)."""
        for _ in range(cycles):
            self.step(n_wr, rd_demand)
        return self.wr_hist[-cycles:], self.exec_hist[-cycles:]

    def stats(self):
        return {
            "cycles": self.cycle,
            "wr_done": self.wr_done,
            "rd_gen": self.rd_gen,
            "rd_done": self.rd_done,
            "backlog": self.backlog,
            "backlog_max": self.backlog_max,
            "lat_avg": round(self.avg_latency, 3),
            "lat_max": self.lat_max,
            "conflict_defer": self.conflict_defer,
            "loop_in_total": self.loop_in_total,
            "loop_out_total": [sum(h) for h in self.loop_out_hist],
            "loop_backlog": [len(self.loop_q0), len(self.loop_q1)],
            "loop_backlog_max": self.loop_backlog_max,
        }
