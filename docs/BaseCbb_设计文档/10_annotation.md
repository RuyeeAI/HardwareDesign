# annotation/ — 后端注解工具（**已删除**）

> ⚠️ chisel 7 移除了 FIRRTL 注解机制（`chisel3.experimental.ChiselAnnotation` 与
> `firrtl.annotations` 随 SFC 一起退出），本目录（`Annotations.scala`）随之删除。
> 原 `SRAMAnnotation` 的唯一消费者 `DescribedSRAM` 改为在 elaboration 期打印描述信息；
> 需要机器可读的 SRAM 清单时，从 firtool 的 `.anno.json` / firrtl 产物侧解决。以下为历史记录。

> 路径：`src/main/scala/BaseCbb/annotation/Annotations.scala`（73 行）　包：`BaseCbb.annotation`（SiFive 风格移植）

---

## 1. FIRRTL 注解

| 注解 | 字段 | 用途 |
|------|------|------|
| `SRAMAnnotation` | target, address_width, name, data_width, depth, description, write_mask_granularity | 记录 SRAM 例化信息（供后端 SRAM 编译器/面积提取） |
| `InterruptsPortAnnotation` | target, name, interruptIndexes | 记录模块的中断号 |
| `GlobalConstantsAnnotation` | target, xLen | 记录全局常量（仅 xLen） |
| `ParamsAnnotation` | target, paramsClassName, params: Map[String,Any] | 反射提取参数化 case class 的字段名+值 |

## 2. Chisel 层包装

- `GlobalConstantsChiselAnnotation(target, xLen)` → `toFirrtl = GlobalConstantsAnnotation(...)`。
- `ParamsChiselAnnotation(target, params: T <: Product)`：`paramMap = 反射 getDeclaredFields 与 productIterator 配对`。

## 3. Annotated — 便捷入口

```scala
Annotated.srams(component, name, address_width, data_width, depth, description, write_mask_granularity)
Annotated.interrupts(component, name, interrupts: Seq[Int])
```

## 4. 设计注意

1. **未使用导入**：`chisel3.RawModule`、`org.json4s.JsonDSL._`、`org.json4s.native.JsonMethods.{pretty, render}`。
2. snake_case（`address_width`）与 camelCase（`interruptIndexes`）混用。
3. 复制粘贴注释（两处 "Record a case class that was used to parameterize this target."）。
4. `ParamsAnnotation.params: Map[String,Any]` 不利于直接 JSON 序列化。
5. 无专门测试文件。
