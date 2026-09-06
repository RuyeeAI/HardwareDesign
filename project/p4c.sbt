// P4C 编译器源码挂入 sbt 元构建，供根 build.sbt 的 sourceGenerators 调用。
// 元构建使用 Scala 2.12，P4C 源码保持 2.12/2.13 双兼容（纯 Scala，无 Chisel 依赖）。
Compile / unmanagedSourceDirectories += baseDirectory.value / ".." / "src" / "main" / "scala" / "P4C"
