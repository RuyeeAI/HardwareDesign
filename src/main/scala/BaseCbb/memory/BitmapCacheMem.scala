package BaseCbb.memory

import chisel3._
import chisel3.util._
import chisel3.stage.ChiselStage

import java.io.PrintWriter

class BitmapCacheMem(
  val n:         Int,
  val cacheSize: Int = 64,
  val memLatency: Int = 1
) extends Module {
  require(n > 0, "Bitmap entries must be positive")
  require(isPow2(cacheSize), "Cache size must be power of 2")
  require(cacheSize <= n, "Cache size must not exceed total entries")
  require(memLatency >= 1, "Memory latency must be >= 1")
  require(n % cacheSize == 0, "n must be divisible by cacheSize")

  val log2N     = log2Ceil(n)
  val log2M     = log2Ceil(n / cacheSize)
  val cacheIdxW = log2Ceil(cacheSize)

  val io = IO(new Bundle {
    val mem = Flipped(new TpMemoryPort(log2M, cacheSize))

    val alloc_req   = Input(Bool())
    val alloc_ptr   = Output(UInt(log2N.W))
    val alloc_valid = Output(Bool())

    val free_req = Input(Bool())
    val free_ptr = Input(UInt(log2N.W))

    val init = Input(Bool())

    val empty   = Output(Bool())
    val full    = Output(Bool())
    val freeCnt = Output(UInt(log2N.W))
  })

  val M = n / cacheSize

  // Cache: stores one row (cacheSize bits)；语义 1 = 可用（与 BitmapKernel 统一）
  val cacheData  = Reg(Vec(cacheSize, Bool()))
  val cacheTag   = Reg(UInt(log2M.W))
  val cacheValid = RegInit(false.B)

  // Current row free count & first free column（1 = 可用）
  val cacheFreeCnt     = BitmapKernel.freeCount(cacheData.asUInt)
  val firstFreeInCache = BitmapKernel.firstFree(cacheData.asUInt)

  // State machine
  val sIdle :: sRead :: sWrite :: sInit :: Nil = Enum(4)
  val state = RegInit(sIdle)

  // Request context
  val req_row  = Reg(UInt(log2M.W))
  val req_col  = Reg(UInt(cacheIdxW.W))
  val req_type = Reg(UInt(2.W))

  // Cycle counter in sRead.
  // On entry: latCnt = 1, condition false (we haven't waited yet).
  // After that: increments each cycle.
  // When latCnt % memLatency === 0 AND we are in sRead: a RAM read just completed — process it.
  val latCnt = RegInit(0.U(log2Ceil(memLatency + 1).W))

  // init counter
  val initRow = RegInit(0.U(log2M.W))

  // Memory interface defaults
  io.mem.we    := false.B
  io.mem.re    := false.B
  io.mem.waddr := 0.U
  io.mem.raddr := 0.U
  io.mem.wdata := 0.U

  // ptr decomposition
  val ptr_row    = io.free_ptr(log2M + cacheIdxW - 1, cacheIdxW)
  val ptr_col    = io.free_ptr(cacheIdxW - 1, 0)
  val ptrInCache = cacheValid && (ptr_row === cacheTag)

  // Combinational next-row
  val nextRow = Mux(req_row === (M - 1).U, 0.U, req_row + 1.U)

  // Output defaults — driven every cycle
  io.alloc_ptr   := 0.U
  io.alloc_valid := false.B

  switch(state) {
    // ======================================================================
    // sIdle
    // ======================================================================
    is(sIdle) {
      // Cache hit: allocate from cached row combinatorially (1 cycle)
      when(cacheValid && BitmapKernel.hasFree(cacheData.asUInt)) {
        val col = firstFreeInCache
        val ptr = Cat(cacheTag, col)
        cacheData(col) := false.B   // 占用 = 清 0
        io.alloc_ptr   := ptr
        io.alloc_valid := true.B
      }

      // Cache miss: start searching from nextRow (combinational path)
      when(io.alloc_req && (!cacheValid || cacheFreeCnt === 0.U)) {
        val startRow = Mux(cacheValid, nextRow, 0.U)
        req_row  := startRow
        req_type := 0.U
        latCnt   := 1.U          // start counting; trigger after memLatency cycles
        state    := sRead
      }

      // Free: hit in cache → update cache; miss → go to sRead to write SRAM
      when(io.free_req) {
        when(ptrInCache) {
          cacheData(ptr_col) := true.B   // 释放 = 置 1
        }.otherwise {
          req_row  := ptr_row
          req_col  := ptr_col
          req_type := 1.U
          latCnt   := 1.U
          state    := sRead
        }
      }

      when(io.init) {
        initRow := 0.U
        state   := sInit
      }
    }

    // ======================================================================
    // sRead — pipelined: read a new row every cycle; process result when
    //           latCnt % memLatency === 0 (i.e., memLatency cycles have passed)
    // ======================================================================
    is(sRead) {
      // Increment counter every cycle.
      // When condition fires: we have waited memLatency cycles and the RAM
      // data from `req_row` is ready. Process it and start reading nextRow.
      latCnt := latCnt + 1.U

      // Drive RAM address every cycle
      io.mem.re    := true.B
      io.mem.raddr := req_row

      when(latCnt % memLatency.U === 0.U) {
        val row_data = io.mem.rdata

        // ---- Allocation search ----
        when(req_type === 0.U) {
          cacheData  := VecInit(row_data.asBools)
          cacheTag   := req_row
          cacheValid := true.B

          val rowFreeCnt = BitmapKernel.freeCount(row_data)
          val rowHasFree = BitmapKernel.hasFree(row_data)

          when(rowHasFree) {
            // Found a non-full row — allocate from it
            val alloc_col = BitmapKernel.firstFree(row_data)
            cacheData(alloc_col) := false.B   // 占用 = 清 0
            io.alloc_ptr   := Cat(req_row, alloc_col)
            io.alloc_valid := true.B
            state := sIdle
          }.otherwise {
            // This row is also full — prepare to check next row
            // nextRow is combinational from req_row; it is stable now
            req_row := nextRow
            latCnt  := 0.U  // reset counter; next cycle it becomes 1 and we
                             // start counting toward the next memLatency
          }
        }

        // ---- Free: read-modify-write ----
        when(req_type === 1.U) {
          val new_row = Wire(Vec(cacheSize, Bool()))
          new_row := VecInit(row_data.asBools)
          new_row(req_col) := true.B   // 释放 = 置 1

          io.mem.we    := true.B
          io.mem.waddr := req_row
          io.mem.wdata := new_row.asUInt
          state := sIdle
        }
      }
    }

    // ======================================================================
    // sWrite
    // ======================================================================
    is(sWrite) {
      io.mem.we    := true.B
      io.mem.waddr := req_row
      io.mem.wdata := cacheData.asUInt
      state := sIdle
    }

    // ======================================================================
    // sInit
    // ======================================================================
    is(sInit) {
      io.mem.we    := true.B
      io.mem.waddr := initRow
      io.mem.wdata := ((BigInt(1) << cacheSize) - 1).U  // 全 1 = 全可用
      when(initRow === (M - 1).U) {
        cacheValid := false.B
        state := sIdle
      }
    }
  }

  // Status outputs —— 缓存行级近似（全局空/满需扫描全部行，此处以缓存行状态表达）
  // 语义与 Bitmap 一致：empty = 全可用（无已分配），full = 全占（无可用）
  io.full  := cacheValid && BitmapKernel.isFull(cacheData.asUInt)
  io.empty := cacheValid && BitmapKernel.isEmpty(cacheData.asUInt)
  io.freeCnt := Mux(cacheValid, cacheFreeCnt, cacheSize.U) +
                Mux(cacheValid, (M - 1).U * cacheSize.U, 0.U)

  printf(p"BitmapCacheMem($n, cache=$cacheSize, lat=$memLatency, M=$M): " +
    p"state=$state alloc_ptr=${io.alloc_ptr} alloc_valid=${io.alloc_valid} " +
    p"empty=${io.empty} full=${io.full}\n")
}

object EmitBitmapCacheMemVerilog {
  def main(args: Array[String]): Unit = {
    val dir = args.headOption.getOrElse("generated")

    val n = 256
    val cacheSize = 64
    val memLatency = 1

    println(s"Emitting BitmapCacheMem(n=$n, cacheSize=$cacheSize, memLatency=$memLatency)...")
    val verilog = ChiselStage.emitSystemVerilog(new BitmapCacheMem(n, cacheSize, memLatency))
    val file = s"$dir/BitmapCacheMem.sv"
    new PrintWriter(file) { write(verilog); close() }
    println(s"Saved to $file (${verilog.length} chars)")
  }
}
