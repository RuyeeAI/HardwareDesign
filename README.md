# HardwareDesign 基础电路单元库 (Chisel 版本)

这个工程使用 Chisel 硬件构造语言实现了数字IC设计中常用的基础电路单元。

## 目录结构

```
HardwareDesign/
├── build.sbt                                      # sbt 构建配置
├── README.md                                      # 本说明文件
└── src/
    └── main/
        └── scala/
            ├── basic/
            │   └── BasicCells.scala               # 基础门级单元
            ├── arithmetic/
            │   └── ArithmeticUnits.scala          # 算术运算单元
            └── sequential/
                └── SequentialUnits.scala          # 时序电路单元
```

## 使用方法

1. 确保你已经安装了 `sbt` 构建工具

2. 生成Verilog代码示例，在sbt shell中运行：
```sbt
runMain basic.Inv
```
这会生成反相器的Verilog代码到 `generated/` 目录

3. 运行测试（需要添加chiseltest）：
```sbt
test
```

## 已包含单元列表

### 基础门级单元 (`basic/BasicCells.scala`)
- Inv: 反相器
- Buf: 缓冲器
- And2/And3: 二/三输入与门
- Nand2/Nand3: 二/三输入与非门
- Or2/Nor2/Nor3: 或门、或非门
- Xor2/Xnor2: 异或门、同或门
- Mux2/Mux2N: 一比特/N比特二选一多路选择器
- Dec2/Dec3: 2-4、3-8译码器
- DLatch: D锁存器
- DFF/DFFAsyncRst/DFFSyncRst: D触发器（支持异步/同步复位）
- HalfAdd/FullAdd: 半加器、全加器
- SRLatch: SR锁存器
- ClockGating: 时钟门控单元
- AOI22/AOI32: 与或非门

### 算术运算单元 (`arithmetic/ArithmeticUnits.scala`)
- RippleCarryAdder: N位逐位进位加法器
- CarrySelectAdder: N位进位选择加法器
- Subtractor: N位减法器
- AddSub: N位可配置加减法器
- Comparator: N位比较器
- Multipler: 行为级乘法器
- LeftShifter/RightShifter: 左/右移位器（支持算术移位）

### 时序电路单元 (`sequential/SequentialUnits.scala`)
- Register: N位同步寄存器
- RegFile1R1W/RegFile2R1W: 1读1写/2读1写寄存器堆
- UpCounter: 二进制递增计数器
- ModNCounter: 模N计数器
- ClkDiv2: 二分频
- ClkDivOdd: 任意奇数分频（50%占空比）
- ClkDiv: 通用整数分频
- SyncFifo: 同时钟域FIFO
- FsmTemplate: 三段式有限状态机模板

## 依赖

- Chisel 3.5+
- Scala 2.13+
