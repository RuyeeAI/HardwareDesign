# misc/ — 通用硬件构件（原 utils/ 主体）

> 路径：`src/main/scala/BaseCbb/misc/`　包：`BaseCbb.misc`（`Timer.scala`/`Shaper.scala` 例外：声明 `BaseCbb.utils.timer`，陈旧包名，见 §11）
> 文件 11 个：Broadcaster / DelayQueue / LatencyPipe / Misc / ReorderQueue / Repeater / Shaper / ShiftQueue / ShiftReg / Timer / utils
> 本目录由原 `utils/` 拆分而来（P3），是**通用硬件构件**库；主机侧工具在 `io/`，数据结构在 `data/`。

---

## 1. 延迟/队列族

### 1.1 LatencyPipe — 固定级数延迟流水线

```scala
class LatencyPipe[T <: Data](typ: T, latency: Int)
// in / out：DecoupledIO
```
- `io.out <> doN(latency, x => Queue(x, 1, pipe=true), io.in)`：`latency` 个深度 1 的 **pipe 队列**串联。
- **弹性延迟**：队列空时组合直通、忙时打拍 —— 实际延迟 ≤ latency，非严格固定周期。
- `RegEn`（同文件）：单拍使能寄存器（`out.bits := RegEnable(in.bits, in.valid)`；`out.valid := RegNext(in.valid)`）；
  `LatencyPipeV` = ValidIO 版 `foldLeft` 串联 `latency` 个 RegEn。
- 工厂：`LatencyPipe.apply(in, latency, instanceName)`、`LatencyPipeV.apply(...)`。

### 1.2 DelayQueue — 运行时可变延迟队列

```scala
class DelayQueue[T <: Data](gen: T, entries: Int)
// enq/deq：Decoupled；timer：UInt（自由计数器）；delay：UInt（目标延迟周期）
```
- 内部 `Queue({data, time}, entries, flow=true)` 存数据+时间戳；出队 `valid = deq.valid && (timer - time) >= delay_r`（无符号环绕减法，timer 回绕正确）。
- `delay` 变化时**断言队列必须为空**（否则未定义行为）。
- 4 个 apply：外置 timer / 内置自由计数器 / 静态延迟 / 动态延迟（maxDelay 默认 4096）。

### 1.3 ReorderQueue — 乱序完成缓冲

```scala
class ReorderQueue[T <: Data](dType: T, tagWidth: Int, size: Option[Int] = None)
// enq：Decoupled{data, tag}；deq：{valid(Input), tag(Input), data(Output), matches(Output)}
```
- **CAM 模式**（`tagSpaceSize > actualSize`）：3 个 Vec 寄存器（data/tags/free），入队地址 = `PriorityEncoder(roq_free)`；支持**同 tag 多条目在途**；deq 取最低地址匹配者。
- **直接索引模式**（`tagSpaceSize <= actualSize`）：`Mem` 按 tag 索引，同 tag 至多一个在途。
- ⚠ 生成期 `println("Warning - using a CAM ...")` 应改 logger。

### 1.4 ShiftQueue — 移位寄存器队列

```scala
class ShiftQueue[T <: Data](gen: T, entries: Int, pipe = false, flow = false)  // 追加 mask: Output UInt(entries.W)
```
- 接口兼容 `chisel3.util.Queue`（继承 QueueIO），内部为**全移位 Vec**：数据每拍整体左移，deq 侧 `bits := elts.head` 零 Mux（deq 快），enq 侧每槽一个 Mux（面积/功耗换延迟）。
- `mask := valid.asUInt`，`count := PopCount(mask)`。
- **适用**：浅/常空 flow-through 流；深队列请用 util.Queue。

### 1.5 延迟族对比

| 模块 | 延迟性质 | 时间戳 | 反压 | 保序 | 适用 |
|------|---------|:---:|:---:|:---:|------|
| `LatencyPipe` | 静态 ≤latency 拍（弹性） | 无 | 有 | 是 | 固定延迟+省面积 |
| `DelayQueue` | 动态（运行时 delay） | 有 | 有 | 是 | 可配置延迟 |
| `ReorderQueue` | 不延迟，按 tag 重排 | 无 | 有 | 否 | 乱序完成缓冲 |
| `ShiftQueue` | FIFO（零延迟出队） | 无 | 有 | 是 | 浅队列 |

---

## 2. 分发/重发族

### 2.1 Broadcaster — 1→N 轮转分发

```scala
class Broadcaster[T <: Data](typ: T, n: Int)
// in：Flipped Decoupled；out：Vec(n, Decoupled)
```
- `n==1` 直连；`n>1` 轮转：`idx` 指针 + `save` 保存当前元素，`in.fire` 时保存并推进指针，`out(i)` 轮流输出。
- **注意**：`in.ready := out.head.ready && idx === 0` —— 输入吞吐受整个轮转周期限制（每 n 拍才收一个新元素）。

### 2.2 Repeater — 重复发送器

```scala
class Repeater[T <: Data](gen: T)
// enq/deq：Decoupled；repeat：Input；full：Output
```
- 默认直通；`enq.fire && repeat` 时保存元素并置 full，之后 `deq.bits := saved` 持续重发，直到 repeat 撤销。
- 用途：持续发送同一报文。

---

## 3. 定时器族（Timer.scala，包 `BaseCbb.utils.timer` 陈旧）

| 类 | 并发 | 周期 | start/stop | 超时输出 | 超时后 |
|----|:---:|------|-----------|---------|--------|
| `Timer(initCount, maxInflight)` | 多事件（ID 位图） | 静态 | Valid(ID) | Valid(ID) | 需 stop；同拍只报最小 ID |
| `SimpleTimer(initCount)` | 单事件 | 静态 | Bool | Bool | 不回绕自停（继续减会回绕） |
| `DynamicTimer(w)` | 单事件 | 动态 period | Bool | Bool | **到 0 自动停** |

- `Timer`：共享 `countdown`，`inflight` 位图；`timeout.bits := PriorityEncoder(inflight)`；`assert(!stop.valid || inflight(stop.bits))`。
- **建议**：`SimpleTimer` 与 `DynamicTimer` 仅差"周期来源"，可删除 SimpleTimer 或让其代理到 `DynamicTimer(period = initCount.U)`。

## 4. Shaper — 令牌桶流量整形（Shaper.scala，包 `BaseCbb.utils.timer` 陈旧）

```scala
class Shaper(tokenWidth = 16)
// rate/burstSize/interval：Input；req/pktSize：Input；pass/tokens：Output
```
- 每 `interval` 拍补充 `rate` 个 token（上限 burstSize）；`pass = req && tokens >= pktSize`，放行扣 token。
- ⚠ 同拍"补 token 与扣 token"：后写减法覆盖先写加法（该拍只减不加）；`interval=0` 时回绕为 65536 拍。

---

## 5. 移位寄存器（ShiftReg.scala）

| 符号 | 说明 |
|------|------|
| `ShiftRegInit.apply(in, n, init, name)` | 级联 `RegNext(next, init)` + 命名（`${name}_${i}`） |
| `ShiftRegInit.apply(in, n, init, en, name)` | 带使能版（吞并原 utils.ShiftRegEn）：每级 `RegEnable`，en 逐级打拍传播 |
| `AbstractPipelineReg(w)` | 抽象单口流水寄存器（d/q），`apply(gen, in, name)` 包装任意 Data |
| `AsyncResetShiftReg(w, depth, init, name)` | 异步复位移位链：`depth` 个 `AsyncResetRegVec` 级联；`desiredName = AsyncResetShiftReg_w{w}_d{depth}_i{init}` |

**跨组重复**：`AsyncResetShiftReg`（misc）与 `async.AsyncResetSynchronizerShiftReg`（utils.cdc）**功能完全相同**（misc 版注释自认 "Functionally identical ... only used for timing applications"）——建议统一实现（misc 版委托 CDC 原语），见重复分析文档 §2.1。

---

## 6. 组合逻辑工具（Misc.scala）

| 对象 | 功能 | 注意 |
|------|------|------|
| `DecoupledHelper` | 多信号握手合成：`fire(exclude, includes*)`（**引用相等**排除） | |
| `MuxT` | 2/3/4 元 tuple 逐元素 Mux | 与 math.MuxLiteral 分工（见重复分析 §3） |
| `MuxTLookup` | key 级联查找（运行时键，线性深度） | 键可为运行时值 |
| `ValidMux` | 多路 ValidIO 合一（or-reduce + MuxCase） | |
| `Str` | ASCII/字符串→UInt；数值→radix 进制 ASCII | 硬件调试打印 |
| `Split` | UInt 按位界拆 2/3/4 段元组 | ⚠ n0=0 越界无防护 |
| `Random` | 硬件随机：2 幂取模 / 非 2 幂无偏（PriorityEncoder+partition）/ oneHot | 随机源 `LFSR(16)` |
| `Majority` | Bool 集合"超过半数" | ⚠ 大集合 `subsets` 指数爆炸 |
| `PopCountAtLeast` | popcount ≥ n 检测（n≤2 特化二分树） | ⚠ **n>3 抛 MatchError**（仅支持 0..3） |
| `MaskGen` | 地址/大小 → 字节使能掩码（含 groupBy 交错归约） | |

---

## 7. 主机侧工具（utils.scala）

| 对象 | 功能 | 注意 |
|------|------|------|
| `GenProcessBuilder(cmd)` | 按空白拆分命令并异步启动（`ProcessBuilder`） | 修复了"整条命令当可执行文件"的旧 bug |
| `Seq2Vec(s)` | `Seq[T] → Wire(Vec)` | **与 chisel3.util.VecInit 功能重复** |
| `SubVec(v, st, size)` | Vec 连续子段 → 新 Vec | 纯组合拷贝 |
| `Convert2dArray(v)` | 二维 Vec 转置 | |

---

## 8. 设计注意与建议

1. **陈旧包名**：`Timer.scala`/`Shaper.scala` 声明 `BaseCbb.utils.timer`，物理在 misc/——建议改为 `BaseCbb.misc`（牵动 13 个测试 spec 的 `import BaseCbb.utils.timer._`，见重复分析文档 §7）。
2. **`PopCountAtLeast` 仅支持 n≤3**，n>3 抛 MatchError；建议补 n=3 用 PopCount、n>3 拒绝或泛化。
3. **`Random.oneHot(mod==1)` 越界**（`log2Up(1)-1 == -1`）。
4. **`FileIO.ReadFile` 未 close Source**（资源泄漏）——见 io/ 文档。
5. `Seq2Vec` 与 `VecInit` 重复：可删除改用 VecInit（需检查全库调用方）。
6. 测试：`src/test/scala/BaseCbb/utils/` 下 BroadcasterSpec / ChecksumSpec(实为 math) / CompressSpec / CountersSpec / CrcSpec / DelayQueueSpec / IDPoolSpec(实为 memory) / LatencyPipeSpec / LfsrSpec / MuxLiteralSpec / ReorderQueueSpec / RepeaterSpec / ShaperSpec / TimerSpec，共约 1000 行。
