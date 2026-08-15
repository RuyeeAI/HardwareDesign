package BaseCbb.async

import chisel3._

/**
 * 异步复位同步释放（Async Reset Synchronizer / Reset Bridge）。
 *
 * 接收 `asyncRst`（异步复位），输出 `syncRst`（AsyncReset）：
 *  - 复位异步有效：`asyncRst` 拉高时两级触发器立即置位，`syncRst` 立即有效；
 *  - 释放同步：`asyncRst` 撤除后，`syncRst` 再经过 `StageNum` 拍才撤除，
 *    消除复位释放的亚稳态。
 */
class AsyncRstSync extends Module {
  val io = IO(new Bundle {
    val asyncRst = Input(AsyncReset())
    val syncRst  = Output(AsyncReset())
  })

  withReset(io.asyncRst) {
    val stage1 = RegInit(true.B)
    val stage2 = RegInit(true.B)
    stage1 := false.B
    stage2 := stage1
    io.syncRst := stage2.asAsyncReset
  }
}
