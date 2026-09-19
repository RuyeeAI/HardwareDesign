// ===========================================================================
// EM（tb 预设）功能波形 testbench
//
//   由 tools/em_tb/run_wave.sh 调用；也可手工跑：
//   $ verilator --binary --timing --trace-fst -Wno-fatal -j 4 \
//               --top-module tb -o em_tb tb_exact_match.v <out>/ExactMatch.v \
//               +define+WAVE_FST && ./obj_dir/em_tb
//
//   ⚠️ 仿真器用 Verilator，**不要用 iverilog**：AgeTable / FreeList 被 firtool 展成
//   "连续赋值里对数组做变量下标读"，iverilog 要求常量下标，编不过。
//   +define+WAVE_FST → 输出 .fst（体积小很多）；不加 → 输出 .vcd（可用 vcd_check.py 检查）。
//
//   ⚠️ 本 TB 与 preset=tb 绑定：端口位宽（key 16 / ad 26 / status 各字段）与
//   $dumpvars 里引用的内部信号名都是 tb 预设专属。
//
// 场景（每个阶段都打时间戳 + 关键观测值，没有波形查看器也能读）：
//   P1 复位 + 存储初始化握手
//   P2 维护口 add（含覆盖写：指纹命中那一路的 KT 读）
//   P3 单次命中查找 / 未插入查找
//   P4 背靠背两个同 KEY（第 1 个 miss 并触发自学习，第 2 个由转发 CAM 命中）
//   P5 连续灌命中流量，检查 rsp.valid 每拍都有（II=1）
//   P6 打开老化，条目超时后被清除（KT/AD 资源归还）
//   P7 打满 HT + OVFC
// ===========================================================================
`timescale 1ns/1ps

module tb;
  // ---------------- 时钟/复位 ----------------
  reg clock = 0;
  always #5 clock = ~clock;          // 10ns 周期（功能波形，非 1.2GHz 时序）

  reg reset = 1;

  // ---------------- DUT 端口 ----------------
  wire        key_ready;
  reg         key_valid = 0;
  reg  [15:0] key_bits  = 0;

  wire        rsp_valid;
  wire [26:0] rsp_bits;

  wire        wr_ready;
  reg         wr_valid = 0;
  reg  [1:0]  wr_op    = 0;
  reg  [15:0] wr_key   = 0;
  reg  [25:0] wr_ad    = 0;

  reg  [25:0] learnAd  = 0;
  reg         learnEn  = 0;
  reg         ageEn    = 0;
  reg         memInit  = 0;
  wire        memInitDone;

  reg  [31:0] crcPoly = 0, crcInit = 0, crcXor = 0;

  wire [6:0]  st_entries;
  wire [15:0] st_insert, st_insFail, st_delete, st_learn, st_ageDrop, st_learnDrop, st_fpClash;
  wire [3:0]  st_ovfcUse, st_fwdUse;
  wire [7:0]  st_ktFree, st_adFree;
  wire        st_lkBusy, st_mtBusy;

  ExactMatch dut (
    .clock(clock), .reset(reset),
    .io_key_ready(key_ready), .io_key_valid(key_valid), .io_key_bits(key_bits),
    .io_rsp_valid(rsp_valid), .io_rsp_bits(rsp_bits),
    .io_wr_ready(wr_ready), .io_wr_valid(wr_valid), .io_wr_bits_op(wr_op),
    .io_wr_bits_key(wr_key), .io_wr_bits_ad(wr_ad),
    .io_learnAd(learnAd), .io_learnEn(learnEn), .io_ageEn(ageEn),
    .io_memInit(memInit), .io_memInitDone(memInitDone),
    .io_crcPoly(crcPoly), .io_crcInit(crcInit), .io_crcXor(crcXor),
    .io_status_entries(st_entries), .io_status_insert(st_insert), .io_status_insFail(st_insFail),
    .io_status_delete(st_delete), .io_status_learn(st_learn), .io_status_ageDrop(st_ageDrop),
    .io_status_learnDrop(st_learnDrop), .io_status_fpClash(st_fpClash),
    .io_status_ovfcUse(st_ovfcUse), .io_status_fwdUse(st_fwdUse),
    .io_status_ktFree(st_ktFree), .io_status_adFree(st_adFree),
    .io_status_lkBusy(st_lkBusy), .io_status_mtBusy(st_mtBusy)
  );

  // ---------------- 波形 ----------------
  initial begin
`ifdef WAVE_FST
    $dumpfile("em_tb.fst");
`else
    $dumpfile("em_tb.vcd");
`endif
    $dumpvars(1, tb);                       // 顶层（含 dut 全部端口）
    $dumpvars(0,
      tb.dut.sState, tb.dut.svcOwns,
      tb.dut.acqV, tb.dut.r1V, tb.dut.d1V, tb.dut.d2V, tb.dut.d3V,
      tb.dut.d1Val_0, tb.dut.d1Val_1, tb.dut.d1Val_2, tb.dut.d1Val_3,
      tb.dut.d1FpSelC, tb.dut.tblHit,
      tb.dut.entryCnt, tb.dut.cntInsert, tb.dut.cntInsFail, tb.dut.cntLearn,
      tb.dut.cntAgeDrop, tb.dut.cntFpClash, tb.dut.cntLrnDrop,
      tb.dut.agPend, tb.dut.ovfcUseCnt, tb.dut.ovScIdx);
  end

  integer rspCnt = 0;
  always @(posedge clock) if (rsp_valid) rspCnt = rspCnt + 1;

  // ---------------- 任务 ----------------
  task doWr(input [1:0] op, input [15:0] k, input [25:0] ad, input [511:0] tag);
    begin
      wr_op = op; wr_key = k; wr_ad = ad; wr_valid = 1'b1;
      @(posedge clock);
      while (wr_ready !== 1'b1) @(posedge clock);   // 保持 valid 直到被接收（别只 poke 一拍）
      wr_valid = 1'b0;
      while (st_mtBusy !== 1'b0) @(posedge clock);
      @(posedge clock);
      $display("[%0t] %0s  条目=%0d 插=%0d 插失败=%0d 删=%0d 指纹撞=%0d KT空闲=%0d",
               $time, tag, st_entries, st_insert, st_insFail, st_delete, st_fpClash, st_ktFree);
    end
  endtask

  task doLookup(input [15:0] k, output hit, output [25:0] ad, input [511:0] tag);
    begin
      key_bits = k; key_valid = 1'b1;
      @(posedge clock);
      while (key_ready !== 1'b1) @(posedge clock);  // 同上
      key_valid = 1'b0;
      wait (rsp_valid === 1'b1);
      hit = rsp_bits[26];
      ad  = rsp_bits[25:0];
      $display("[%0t] %0s  key=0x%04h -> hit=%0d ad=0x%0h", $time, tag, k, hit, ad);
      @(posedge clock);
    end
  endtask

  // ---------------- 主场景 ----------------
  reg        hit; reg [25:0] ad;
  integer    i, cnt0, cnt1, guard;
  reg [15:0] keys [0:3];
  reg [15:0] k;

  initial begin
    keys[0] = 16'h0101; keys[1] = 16'h0202; keys[2] = 16'h0303; keys[3] = 16'h0404;

    // ---- P1 复位 + 初始化 ----
    repeat (4) @(posedge clock);
    reset = 0;
    repeat (2) @(posedge clock);
    $display("[%0t] P1 复位完成，开始存储初始化", $time);
    memInit = 1'b1; @(posedge clock); memInit = 1'b0;
    guard = 0;
    while (memInitDone !== 1'b1 && guard < 5000) begin @(posedge clock); guard = guard + 1; end
    $display("[%0t] P1 memInitDone=%0d（等 %0d 拍）条目=%0d KT空闲=%0d",
             $time, memInitDone, guard, st_entries, st_ktFree);

    // ---- P2 add ----
    doWr(2'd0, 16'h1234, 26'h2ABCDE, "P2 add(0x1234)");
    doWr(2'd0, 16'h1234, 26'h0777,   "P2 add(0x1234) 覆盖");

    // ---- P3 命中查找 ----
    doLookup(16'h1234, hit, ad, "P3 lookup");
    doLookup(16'h9999, hit, ad, "P3 lookup(未插入)");

    // ---- P4 背靠背两个同 KEY（需求 2）----
    learnEn = 1'b1; learnAd = 26'h15;
    wait (key_ready === 1'b1);
    key_bits = 16'h0BAD; key_valid = 1'b1;
    @(posedge clock);                        // 请求 A 握手
    if (key_ready === 1'b1) begin
      @(posedge clock);                      // 请求 B 背靠背握手（ready 仍为 1）
    end else begin
      $display("[%0t] P4 !! 第 2 拍 key_ready=0，背靠背失败", $time);
    end
    key_valid = 1'b0;
    for (i = 0; i < 8; i = i + 1) begin
      @(posedge clock);
      if (rsp_valid === 1'b1)
        $display("[%0t] P4 第%0d个响应 hit=%0d ad=0x%0h", $time, i + 1, rsp_bits[26], rsp_bits[25:0]);
    end
    @(posedge clock); @(posedge clock);
    learnEn = 1'b0;
    $display("[%0t] P4 转发CAM占用=%0d 学习次数=%0d", $time, st_fwdUse, st_learn);

    // ---- P5 连续灌命中流量（需求 1：II=1）----
    key_valid = 1'b1;
    // 先填充流水线（查找延迟 4 拍 + 余量），填充期间不计数
    for (i = 0; i < 6; i = i + 1) begin
      key_bits = keys[i % 4];
      @(posedge clock);
    end
    cnt0 = rspCnt;
    for (i = 0; i < 40; i = i + 1) begin
      key_bits = keys[i % 4];
      @(posedge clock);
    end
    key_valid = 1'b0;
    cnt1 = rspCnt - cnt0;
    $display("[%0t] P5 填充后 40 拍内收到 %0d 个响应（svc 无活儿时应为 40，即 II=1；有抢拍则少）",
             $time, cnt1);
    repeat (8) @(posedge clock);

    // ---- P6 老化 ----
    $display("[%0t] P6 打开老化：条目=%0d KT空闲=%0d（timeout=4 tick × tickDiv=8 = 32 拍）",
             $time, st_entries, st_ktFree);
    ageEn = 1'b1;
    guard = 0;
    while (st_entries > 0 && guard < 5000) begin @(posedge clock); guard = guard + 1; end
    $display("[%0t] P6 老化完成：等 %0d 拍，条目=%0d KT空闲=%0d AD空闲=%0d 老化删除=%0d",
             $time, guard, st_entries, st_ktFree, st_adFree, st_ageDrop);
    ageEn = 1'b0;
    doLookup(16'h1234, hit, ad, "P6 老化后查(应 miss)");

    // ---- P7 打满 HT + OVFC ----
    cnt0 = {16'h0, st_insFail};
    for (i = 0; i < 80; i = i + 1) begin
      k = 16'h4000 + i[15:0];
      wait (wr_ready === 1'b1);
      wr_op = 2'd0; wr_key = k; wr_ad = i[25:0]; wr_valid = 1'b1;
      @(posedge clock);
      wr_valid = 1'b0;
      while (st_mtBusy !== 1'b0) @(posedge clock);
    end
    $display("[%0t] P7 插 80 条：条目=%0d OVFC占用=%0d 指纹撞=%0d 插失败=%0d(本次 %0d)",
             $time, st_entries, st_ovfcUse, st_fpClash, st_insFail, st_insFail - cnt0);
    doLookup(16'h4000, hit, ad, "P7 查最早插入(应 hit)");
    doLookup(16'h4001, hit, ad, "P7 再查一条");

`ifdef NO_WAVE
    $display("[%0t] ==== 仿真结束（本次未开启波形 dump）====", $time);
`elsif WAVE_FST
    $display("[%0t] ==== 波形 dump 结束，写入 em_tb.fst ====", $time);
`else
    $display("[%0t] ==== 波形 dump 结束，写入 em_tb.vcd ====", $time);
`endif
    $finish;
  end

  initial begin
    #4_000_000;                              // 兜底：40us 未结束就停
    $display("[%0t] 超时退出", $time);
    $finish;
  end
endmodule
