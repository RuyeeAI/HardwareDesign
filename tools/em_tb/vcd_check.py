#!/usr/bin/env python3
"""EM 波形的自动检查（只吃 VCD；FST 是二进制，请用 Surfer 看）

检查项：
  1) **响应平衡**：#(key 握手) == #(rsp_valid 高电平拍)。
     来历：svc 抢时隙会冻结整条流水线（adv=0），若 rsp_valid 没被 adv 门控，
     停在 d3 的响应会被保持多拍 —— 下游按 valid 计数就把一个请求数成多个响应。
     这个缺陷就是 review 波形时这样发现的，所以留成回归检查。
  2) rsp_valid 最长连续高拍数（连续流下正常会很长，仅作参考）。

用法: python3 vcd_check.py out/em_tb/em_tb.vcd   # 先 tools/em_tb/run_wave.sh --vcd

注：Verilator 会把同名网合并（TB 侧的 rsp_valid 与 DUT 的 io_rsp_valid 是同一个网），
   所以按"多候选名"查找；**任一必需信号找不到就直接报错**，不会静默给出 0 的假通过。
"""
import re
import sys


def vcd_names(path):
    """返回 {id: name} 与 {name: id}（都在 $enddefinitions 之前收集）。"""
    by_id, by_name = {}, {}
    with open(path) as f:
        for ln in f:
            s = ln.strip()
            if s.startswith("$var"):
                p = s.split()
                if len(p) >= 5:                       # $var wire 1 <id> <name> $end
                    by_id[p[3]] = p[4]
                    by_name.setdefault(p[4], p[3])
            elif s.startswith("$enddefinitions"):
                break
    return by_id, by_name


def parse_batches(path, by_id):
    """按时间戳切批：[(time, {id: value})]（只保留本 VCD 里声明过的 id）。"""
    batches, cur, t = [], {}, 0
    body = False
    with open(path) as f:
        for ln in f:
            s = ln.strip()
            if not s:
                continue
            if not body:
                if s.startswith("$enddefinitions"):
                    body = True
                continue
            if s[0] == "#":
                if cur:
                    batches.append((t, cur))
                    cur = {}
                t = int(s[1:])
            elif s[0] in "01xz" and s[1:] in by_id:
                cur[s[1:]] = s[0]              # 键就是 VCD id，别再转名字
            elif s[0] in "bB":
                m = re.match(r"[bB]([01xz]+)\s+(\S+)", s)
                if m and m.group(2) in by_id:
                    cur[m.group(2)] = m.group(1)
    if cur:
        batches.append((t, cur))
    return batches


def main(path):
    by_id, by_name = vcd_names(path)

    # 注意：同一个网可能有多个名字（TB 侧的 key_valid 与 DUT 的 io_key_valid 是同一个 id），
    # 而值变化只带 id —— 所以定位一律用 **id**，不能用名字（否则会查一个永远不更新的别名）。
    def pick(*cands):
        for c in cands:
            if c in by_name:
                return by_name[c]
        print(f"  ❌ 波形里找不到信号 {cands}（Verilator 会合并同名网，"
              f"可用 surfer 打开确认实际名字）", file=sys.stderr)
        sys.exit(1)

    CLK = pick("clock", "io_clock")
    KV = pick("key_valid", "io_key_valid")
    KR = pick("key_ready", "io_key_ready")
    RV = pick("rsp_valid", "io_rsp_valid")
    print(f"信号 id: clk={CLK} key_valid={KV} key_ready={KR} rsp_valid={RV}"
          f"（{by_id[KV]} / {by_id[RV]} 等别名已归一）")

    state, key_hs, rsp_cyc, runs, cur_run = {}, 0, 0, [], 0
    for (t, ch) in parse_batches(path, by_id):
        new = dict(state)
        new.update(ch)
        if state.get(CLK) == "0" and new.get(CLK) == "1":          # posedge
            if state.get(KV) == "1" and state.get(KR) == "1":
                key_hs += 1
            if state.get(RV) == "1":
                rsp_cyc += 1
                cur_run += 1
            else:
                if cur_run:
                    runs.append(cur_run)
                cur_run = 0
        state = new
    if cur_run:
        runs.append(cur_run)

    print(f"  key 握手次数              = {key_hs}")
    print(f"  rsp_valid 高电平拍数      = {rsp_cyc}")
    print(f"  rsp_valid 最长连续高拍数  = {max(runs) if runs else 0}"
          f"（连续流下正常会很长，仅作参考）")
    if key_hs == rsp_cyc:
        print("  ✅ 响应平衡：每个请求恰好一拍响应")
        return 0
    print(f"  ❌ 响应不平衡：{key_hs} 个请求 vs {rsp_cyc} 拍响应（差 {rsp_cyc - key_hs}）"
          f" —— 检查 rsp.valid 是否被 adv 门控")
    return 1


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(__doc__)
        sys.exit(2)
    sys.exit(main(sys.argv[1]))
