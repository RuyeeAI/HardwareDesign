# hardware-design

基于 Chisel 的硬件电路设计项目

## 环境要求

- JDK 11+
- sbt 1.9.x
- Scala 2.13.12

## 项目结构

```
.
├── build.sbt          # sbt 构建配置
├── project/
│   └── build.properties  # sbt 版本定义
└── src/
    ├── main/
    │   └── scala/     # 主要设计代码
    └── test/
        └── scala/     # 测试代码
```

## 使用方法

### 生成 Verilog

```bash
sbt "runMain gcd.GCD"
```

### 运行测试

```bash
sbt test
```

## 依赖版本

- Chisel: 6.2.0
- Scala: 2.13.12
- sbt: 1.9.7
