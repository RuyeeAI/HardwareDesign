#!/usr/bin/env bash
# ===========================================================================
# EM 波形一键生成（preset = tb）
#
#   tools/em_tb/run_wave.sh              # 出 FST 并直接起 Surfer（审查流程）
#   tools/em_tb/run_wave.sh --vcd        # 出 VCD（体积大，可用 vcd_check.py 自动检查）
#   tools/em_tb/run_wave.sh --no-open    # 只跑仿真，不出图形界面（CI/脚本）
#
# 流程：EmGen 产出 Verilog → Verilator 编译 + 跑 testbench → 出波形 →（可选）起 Surfer
# 波形与编译产物落在 out/em_tb/（.gitignore 内）
# 视图预设见 tools/em_tb/em_wave.sucl，testbench 场景说明见 tools/em_tb/README.md
# ===========================================================================
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
PRESET=tb                              # TB 与该预设绑定（端口位宽 + 内部信号名）
VDIR="${REPO_ROOT}/out/em/${PRESET}"       # EmGen 产出的 Verilog 目录
WDIR="${REPO_ROOT}/out/em_tb"            # 波形 + obj_dir
FMT=fst
OPEN_GUI=1
JOBS="$(sysctl -n hw.ncpu 2>/dev/null || echo 4)"

usage() {
  cat <<EOF
用法: $(basename "$0") [选项]

  --fst        输出 FST（默认；体积比 VCD 小很多）
  --vcd        输出 VCD（可用 tools/em_tb/vcd_check.py 做自动检查）
  --no-open    不自动启动 Surfer
  -h, --help   显示本帮助

产物: \$REPO/out/em_tb/em_tb.{fst|vcd}
EOF
}

while [ $# -gt 0 ]; do
  case "$1" in
    --fst)     FMT=fst ;;
    --vcd)     FMT=vcd ;;
    --no-open) OPEN_GUI=0 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "未知参数: $1" >&2; usage >&2; exit 1 ;;
  esac
  shift
done

# ---- 工具链检查（缺什么直接说清楚，别让人猜）----
command -v sbt >/dev/null      || { echo "缺 sbt：请先装 sbt（本仓库用 sbt 构建 Chisel）" >&2; exit 1; }
command -v verilator >/dev/null || { echo "缺 verilator：brew install verilator" >&2; exit 1; }
if [ "$OPEN_GUI" = 1 ]; then
  command -v surfer >/dev/null || { echo "缺 surfer：brew install surfer（或加 --no-open 只跑仿真）" >&2; exit 1; }
fi

WAVE_FILE="${WDIR}/em_tb.${FMT}"
mkdir -p "${WDIR}"

echo "== [1/4] 生成 Verilog（preset=${PRESET}）============================"
(cd "${REPO_ROOT}" && sbt -batch "runMain em.EmGen out/em ${PRESET}") | grep -E '^\[em\]' || true
[ -f "${VDIR}/ExactMatch.v" ] || { echo "没生成 ${VDIR}/ExactMatch.v" >&2; exit 1; }

echo
echo "== [2/4] Verilator 编译（格式 ${FMT}，${JOBS} 线程）=================="
if [ "${FMT}" = fst ]; then
  TRACE_FLAG=--trace-fst; DEF_FLAG=+define+WAVE_FST
else
  TRACE_FLAG=--trace;     DEF_FLAG=+define+WAVE
fi
verilator --binary --timing "${TRACE_FLAG}" -Wno-fatal -j "${JOBS}" \
          --top-module tb -Mdir "${WDIR}/obj_dir" -o em_tb \
          "${SCRIPT_DIR}/tb_exact_match.v" "${VDIR}/ExactMatch.v" "${DEF_FLAG}" \
  > "${WDIR}/verilator.log" 2>&1 || { echo "Verilator 失败，日志：${WDIR}/verilator.log" >&2; tail -20 "${WDIR}/verilator.log" >&2; exit 1; }

echo
echo "== [3/4] 跑 testbench（7 个场景阶段）============================="
(cd "${WDIR}" && ./obj_dir/em_tb)
[ -f "${WAVE_FILE}" ] || { echo "没产出波形 ${WAVE_FILE}" >&2; exit 1; }

echo
echo "== [4/4] 波形：${WAVE_FILE} ($(du -h "${WAVE_FILE}" | cut -f1)) =="
if [ "${FMT}" = vcd ]; then
  echo "   自动检查：python3 ${SCRIPT_DIR}/vcd_check.py ${WAVE_FILE}"
fi
if [ "$OPEN_GUI" = 1 ]; then
  echo "   启动 Surfer（关掉窗口后本脚本才返回）..."
  exec surfer --command-file "${SCRIPT_DIR}/em_wave.sucl" "${WAVE_FILE}"
else
  echo "   查看：surfer --command-file ${SCRIPT_DIR}/em_wave.sucl ${WAVE_FILE}"
fi
