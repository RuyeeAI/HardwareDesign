# GranularExtract：按 G 粒度从 N bit 中取 M bit 的自动选型模块

> 配套实现：`src/main/scala/BaseCbb/align/GranularExtract.scala`
> 配套测试：`src/test/scala/BaseCbb/align/GranularExtractSpec.scala`
> 评估日期：2026-08-28（与「工程优化」同日）

---

## 1. 问题与约束

从 N bit 输入中取出 M bit，起点按粒度 G 对齐：`start = s·G`，`s ∈ [0, S)`，
`S = (N−M)/G + 1`（可选窗口数）。要求 `N ≥ M` 且 `off < S`。

**本模块增加的约束**：`N`、`M` 均为 `G` 的整数倍（`N = n·G`，`M = m·G`）。
因此问题精确等价于 **「从 n 个 G-bit chunk 中选 m 个连续 chunk」**——chunk 是
不可拆分的传输单位，所有偏移都落在 chunk 边界上。

语义（黄金模型，测试据此验证）：

```
out[j] = in[start + j],  j ∈ [0, M)      ⇔      out = (in >> (off·G))(M-1, 0)
```

---

## 2. 候选方案一览

| 方案 | 结构 | 面积（等效 2:1 mux 数，mux2） | 组合深度 | 适用 |
|---|---|---:|---:|---|
| **B 窗口化二分块树** | off 二进制分解，K 级「保持/下移 2^k·G」mux，窗口 N→M 收缩 | `K·M + G·(2^K−1−K)` | K 级 | **默认** |
| **T2 bitmap AND-OR 平面** | off 译码 S 路 one-hot，每输出位一个 S:1 独热 mux | `M·(S−1) + S·K` | 译码+AND+log₂S OR | S 极小且 G 大 |
| T3 整宽桶形/循环移位 | 对整条 N bit 总线做 K 级桶形（=A′） | ≈ `N·K` | K 级 | 不应单独用 |
| T1 字面 AND-fold（有损） | mask 全 N + AND + 折叠 | ≈ `2N` | log₂M | 仅滑窗匹配 |
| E/F 流式/SRAM | 串行交付或 chunk 可寻址存储 | 趋于 0（选择逻辑） | — | 数据本就流式 |

> **关于 T3 的重要结论**：用户上一轮描述的「bitmap + 相与 + 折叠 + G 粒度循环移位」
> 中，循环移位这一步本身就是一个整宽桶形（≈`N·K` mux2），无论 G 取多少都恒定比 B
> 贵约 `N/M` 倍——因为它对**整条总线**移位，而 B 只对**收缩到 M 的窗口**移位。
> bitmap 与循环移位都编码了同一份偏移 = 同一套选择网络买两遍。所以 T3 恒为最贵，
> 模块不实现它（它等价于 A′，不是独立方案）。

代价统一用 **等效 2:1 mux 数（mux2）** 表示；换算到工艺面积用库的
`BaseCbb.Area.ProcessConfiguration.pd_mux2_area`（µm²/mux2），1 个 mux2 ≈ 3 GE。

---

## 3. 代价公式推导

### B 窗口化二分块树
`K = ⌈log₂ S⌉`。第 k 级（k 从 K−1 到 0）的 mux 宽 = `M + (2^k − 1)·G`
（窗口从 `M+(2^K−1)·G` 逐级收缩到 `M`）。面积：

```
Σ_k (M + (2^k−1)·G) = K·M + G·(2^K − 1 − K)
```

选通就是 `off` 的二进制位，**不需要 one-hot 译码**；每级天然可插寄存器流水
（延迟 +K 拍，吞吐不变）。

### T2 bitmap AND-OR 平面
`off` 译码为 S 路 one-hot `sel`；每个输出位 = `OR_s (sel_s ∧ in[c+s])`，即一个
S:1 独热 mux（≈ `S−1` 个 mux2）。译码 `UIntToOH` 额外 `S·K` mux2：

```
M·(S−1) + S·K
```

### 渐近复杂度
- B：`Θ(M·log S + N)`（只走对数）
- T2：`Θ(M·S) = Θ(M·N/G)`（随 G 减小线性爆炸）

---

## 4. 多组 N/M/G 代价对比（统一 mux2 单位）

| N / M / G | n,m,S(K) | B (mux2) | T2 (mux2) | T2/B | 自动选 |
|---|---|---:|---:|---:|---|
| 64 / 32 / 16 | 4,2,3(2) | 80 | 70 | **0.875** | **T2** |
| 64 / 32 / 8 | 8,4,5(3) | 128 | 143 | 1.117 | B |
| 64 / 32 / 4 | 16,8,9(4) | 172 | 292 | 1.70 | B |
| 128 / 32 / 8 | 16,4,13(4) | 216 | 436 | 2.02 | B |
| 256 / 64 / 32 | 8,2,7(3) | 320 | 405 | 1.27 | B |
| 256 / 64 / 16 | 16,4,13(4) | 432 | 820 | 1.90 | B |
| 512 / 128 / 64 | 8,2,5(3) | 640 | 527 | **0.82** | **T2** |
| 512 / 96 / 8 | 64,12,53(6) | 1032 | 5310 | 5.15 | B |
| 512 / 64 / 8 | 64,8,57(6) | 840 | 3926 | 4.67 | B |
| 1024 / 64 / 32 | 32,2,31(5) | 1152 | 2075 | 1.80 | B |
| 1024 / 256 / 64 | 16,4,13(4) | 1728 | 3124 | 1.81 | B |

> **对之前对话结论的修正**：此前（未加约束的分析）曾估「T2 在 S≤8 胜出、交叉点
> S≈7–10」，那是把 B 乘了 3（mux2→GE）而 T2 用原始 mux2 计数造成的**单位不一致**。
> 用统一 mux2 单位后，正确结论是：**B 在绝大多数现实参数下更小；T2 仅在
> S 很小（≈3–5）且 G 较大（G 与 M 可比）时才反超**。工程上默认选 B 是稳妥的。

---

## 5. 交叉点分析

令两式相等：`K·M + G·(2^K−1−K) = M·(S−1) + S·K`。

- **G 大（粗对齐）**：B 的 `G·(2^K−1−K)` 项随 G 线性膨胀，T2 的优势窗口扩大。
  例 `512/128/64`（S=5）T2 省 18%。
- **G 小（细对齐）**：B 优势急剧放大。`512/96/8`（S=53）T2 贵 5.15×，`512/64/8`
  （S=57）贵 4.67×。
- **M 大（宽输出）**：T2 的 `M·(S−1)` 主导，B 几乎总赢。
- **经验法则**：`S ≤ 5 且 G ≳ M/4` 时评估 T2；否则直接 B。

模块据此自动决策，并允许 `prefer=tree|bitmap` 强制覆盖（用于时序/布线调优）。

---

## 6. GranularExtractAuto 模块设计

### IO

```scala
val io = IO(new Bundle {
  val in      = Input(UInt(N.W))
  val off     = Input(UInt(K.W))   // G 粒度偏移，调用方保证 off < S
  val out     = Output(UInt(M.W))
  val sideIn  = Input(UInt(n.W))   // per-chunk 1 位标志（n 个输入 chunk）
  val sideOut = Output(UInt(m.W))  // 选中的 m 个输出 chunk 标志
})
```

- `off` 位宽 `K = ⌈log₂ S⌉`，无译码开销。
- **sideband**：valid/last/keep 等「每 chunk 1 位」标志，宽 n→m，走同一套按 chunk
  移位的网络（内部等价于 `GranularExtract(n, m, G=1)`），与数据严格对齐。

### 自动决策

```scala
val mux2Tree   = K*M + G*((1L<<K) - 1 - K)
val mux2Bitmap = M*(S-1) + S*K
val useTree = prefer match {
  case "tree"   => true
  case "bitmap" => false
  case "auto"   => mux2Tree <= mux2Bitmap
}
```

仅 `off` 的比特数（而非运行值）参与选型——决策在 elaboration 期确定，零运行时开销。

### 两种实现的输出一致性

B 与 T2 数学上等价（均实现 `out = (in >> off·G)(M-1,0)`），测试以同一黄金模型
对两种实现做随机比对，断言逐位相等（见 §7）。

### 流水化

B 每级是 2:1 mux，可在级间插入 `RegNext` 把组合深度从 K 级降到 1 级/拍
（延迟 +K 拍，吞吐不变）。T2 的译码+OR 树亦可在 OR 级间流水。模块当前为纯组合，
流水为后续可选增强。

### 非法 off（off ≥ S）

`in` 高位已补 0 且 bitmap 的越界项置 `false.B`——均为 don't-care，不耗面积；
调用方须保证 `off < S`。

---

## 7. 验证策略

`GranularExtractSpec`（15 用例，全部通过）：

1. **黄金模型回环**：8 组 N/M/G × 200 随机 off，断言 `out == (in>>off·G)(M-1,0)`。
2. **自动选型断言**：`512/96/8 → tree`、`64/32/8 → tree`、`64/32/16 → bitmap`。
3. **两实现一致性**：tree vs bitmap 随机比对，断言逐位相同（`512/96/8`、`64/32/8`）。
4. **sideband 对齐**：`256/64/32` 下 sideOut 按 chunk 移位与黄金模型一致。
5. **prefer 覆盖**：`prefer=tree/bitmap` 强制生效。

```
sbt "testOnly BaseCbb.align.GranularExtractSpec"
```

---

## 8. FPGA 备注

- LUT6 原生 4:1 mux，桶形树每 2 级合并进 1 个 LUT，B 与 T2 的相对优势缩小。
- 流式方案在 FPGA 上更强：SRL（移位寄存器 LUT）实现滑窗近乎免费，若上游本就
  每拍 G bit 流进，优先 E 而非 Mux 类。
- T2 的大扇出（每个输入位驱动最多 S 个 AND）在 FPGA 上由布线器吸收，但大 N 下
  仍可能成为时序热点。

---

## 9. 落地与选型建议

1. **数据整块到达 + 运行时变偏移** → 用 `GranularExtractAuto`（默认 auto）。
2. **S 极小（相邻 chunk、S≤3）且 G 较大** → 评估 `prefer=bitmap`（省 10–20%）。
3. **输入是 G/cycle 流** 或 **已落在 chunk 可寻址存储** → E/F（选择逻辑趋零）。
4. **检测而非提取**（滑窗匹配）→ T1（≈2N 有损 AND-fold），比「提取+判决」省。
5. **不要**在提取器里加「整宽循环移位」步骤（T3）——它恒比 B 贵约 N/M 倍。

模块已接入 `BaseCbb/align/`，复用 `GenModule` 基类的 `desiredName`/参数化约定，
可直接被寄存器生成工具与顶层整合。
