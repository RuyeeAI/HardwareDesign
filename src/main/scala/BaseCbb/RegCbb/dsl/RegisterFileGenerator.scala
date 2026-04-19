package BaseCbb.RegCbb.dsl

import chisel3._
import chisel3.util._
import BaseCbb.utils.AsyncResetReg

/** Memory port interface for direct access */
class MemPortIO(val addrWidth: Int, val dataWidth: Int) extends Bundle {
  val we    = Input(Bool())
  val re    = Input(Bool())
  val waddr = Input(UInt(addrWidth.W))
  val raddr = Input(UInt(addrWidth.W))
  val wdata = Input(UInt(dataWidth.W))
  val rdata = Output(UInt(dataWidth.W))
}

/** Register file IO bundle */
class RegisterFileIO(
  val addrWidth: Int = 12,
  val dataWidth: Int = 32,
  val numMemories: Int = 0,
  val numRegisters: Int = 0
) extends Bundle {
  // Register interface
  val reg_addr   = Input(UInt(addrWidth.W))
  val reg_wrEn   = Input(Bool())
  val reg_wrData = Input(UInt(dataWidth.W))
  val reg_rdEn   = Input(Bool())
  val reg_rdData = Output(UInt(dataWidth.W))

  // Memory interface
  val mem_addr   = Input(UInt(addrWidth.W))
  val mem_wrEn   = Input(Bool())
  val mem_wrData = Input(UInt(64.W))
  val mem_rdEn   = Input(Bool())
  val mem_rdData = Output(UInt(64.W))
  val mem_cs     = Input(Bool())

  // Direct register outputs (flat)
  val regDataOut = Output(Vec(numRegisters, UInt(32.W)))
}

/** Hardware generator for register file */
object RegisterFileGenerator {
  def generate(map: AddressMap)(implicit addrWidth: Int = 12, dataWidth: Int = 32) = {
    val mod = Module(new RegisterFileModule(map)(addrWidth, dataWidth))
    (mod.io, mod.io.regDataOut, mod.memPortIO)
  }
}

class RegisterFileModule(map: AddressMap)(implicit addrWidth: Int, dataWidth: Int) extends Module {
  val numRegs = map.registerAllocations.size
  val numMemories = map.memoryAllocations.size
  val io = IO(new RegisterFileIO(addrWidth, dataWidth, numMemories, numRegs))

  // ============================================================================
  // Register Storage
  // ============================================================================
  val registerStorage = map.registerAllocations.map { regAlloc =>
    val totalBits = regAlloc.register.fields.map(_.bitWidth).sum
    val resetVal = regAlloc.register.fields.head.resetValue
    val storage = RegInit(resetVal.U(totalBits.W))
    storage.suggestName(s"reg_${regAlloc.register.name}")
    storage
  }

  // ============================================================================
  // Memory Storage
  // ============================================================================
  val memoryStorage = map.memoryAllocations.map { memAlloc =>
    val mem = memAlloc.memory
    val storage = Mem(mem.depth, UInt(mem.dataWidth.W))
    storage.suggestName(s"mem_${mem.name}")
    (storage, mem)
  }

  // ============================================================================
  // Register Address Decoder
  // ============================================================================
  private val regAddrMatch = Wire(Vec(numRegs, Bool()))
  for ((regAlloc, i) <- map.registerAllocations.zipWithIndex) {
    val absoluteAddr = map.block.regBaseAddress + regAlloc.byteOffset
    regAddrMatch(i) := io.reg_addr === absoluteAddr.U(addrWidth.W)
  }
  private val regSelected = regAddrMatch.asUInt.orR

  // ============================================================================
  // Memory Address Decoder
  // ============================================================================
  private val memSelect = Wire(Vec(numMemories, Bool()))
  for ((memAlloc, i) <- map.memoryAllocations.zipWithIndex) {
    val mem = memAlloc.memory
    val inRange = io.mem_addr >= mem.baseAddress.U(addrWidth.W) &&
                  io.mem_addr < (mem.baseAddress + mem.byteSize).U(addrWidth.W)
    memSelect(i) := io.mem_cs && inRange
  }

  // ============================================================================
  // Register Write Logic
  // ============================================================================
  when(io.reg_wrEn && regSelected) {
    for ((regAlloc, i) <- map.registerAllocations.zipWithIndex) {
      when(regAddrMatch(i)) {
        var bitPos = 0
        for ((fieldAlloc, fidx) <- regAlloc.fieldAllocations.zipWithIndex) {
          val fieldWidth = fieldAlloc.field.bitWidth
          val fieldBits = io.reg_wrData(bitPos + fieldWidth - 1, bitPos)

          // Only write to writable fields
          fieldAlloc.field.access match {
            case AccessType.WO | AccessType.RW | AccessType.RC =>
              registerStorage(i) := updateFieldValue(
                registerStorage(i),
                bitPos,
                fieldWidth,
                fieldBits,
                fieldAlloc.field.writeAction
              )
            case _ =>
          }
          bitPos += fieldWidth
        }
      }
    }
  }

  // ============================================================================
  // Register Read Logic
  // ============================================================================
  io.reg_rdData := 0.U
  for ((regAlloc, i) <- map.registerAllocations.zipWithIndex) {
    when(regAddrMatch(i) && io.reg_rdEn) {
      val readData = Wire(UInt(regAlloc.register.totalBits.W))
      readData := registerStorage(i)

      // For RC (read-to-clear) fields, auto-clear on read
      var bitPos = 0
      for ((fieldAlloc, fidx) <- regAlloc.fieldAllocations.zipWithIndex) {
        val fieldWidth = fieldAlloc.field.bitWidth
        if (fieldAlloc.field.access == AccessType.RC) {
          registerStorage(i) := updateFieldValue(
            registerStorage(i),
            bitPos,
            fieldWidth,
            0.U(fieldWidth.W),
            WriteAction.Normal
          )
        }
        bitPos += fieldWidth
      }
      io.reg_rdData := readData
    }
  }

  // ============================================================================
  // Memory Read/Write Logic
  // ============================================================================
  private val memRdData = Wire(Vec(numMemories, UInt(64.W)))

  for ((memAlloc, i) <- map.memoryAllocations.zipWithIndex) {
    val (storage, mem) = memoryStorage(i)
    val memOffset = io.mem_addr - mem.baseAddress.U(addrWidth.W)
    val memAddr = memOffset(log2Ceil(mem.depth) - 1, 0)

    // Write
    when(memSelect(i) && io.mem_wrEn) {
      storage.write(memAddr, io.mem_wrData(mem.dataWidth - 1, 0))
    }

    // Read (registered output)
    val readData = storage.read(memAddr)
    memRdData(i) := Cat(0.U((64 - mem.dataWidth).W), readData)
  }

  // Memory read mux
  io.mem_rdData := MuxCase(0.U(64.W), memSelect.zip(memRdData).map { case (sel, data) =>
    sel -> data
  })

  // ============================================================================
  // Direct Register Data Output
  // ============================================================================
  io.regDataOut := VecInit(registerStorage.map { storage =>
    storage
  })

  // ============================================================================
  // Direct Memory Access Interface (memPortIO)
  // ============================================================================
  val memPortIO = IO(Vec(numMemories, new Bundle {
    val we    = Input(Bool())
    val re    = Input(Bool())
    val waddr = Input(UInt(10.W))
    val raddr = Input(UInt(10.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
  }))

  for (((memAlloc, memIdx), portIO) <- map.memoryAllocations.zipWithIndex.zip(memPortIO)) {
    val (storage, mem) = (memoryStorage(memIdx)._1, memoryStorage(memIdx)._2)

    when(portIO.we) {
      storage.write(portIO.waddr, portIO.wdata)
    }
    portIO.rdata := storage.read(portIO.raddr)
  }

  // ============================================================================
  // Helper Functions
  // ============================================================================
  private def updateFieldValue(
    reg: UInt,
    bitPos: Int,
    width: Int,
    newVal: UInt,
    action: WriteAction
  ): UInt = {
    val mask = (~0.U(width.W)) << bitPos
    action match {
      case WriteAction.Normal =>
        (reg & ~mask) | (newVal << bitPos)
      case WriteAction.OneToClear =>
        reg & ~(newVal << bitPos)
      case WriteAction.OneToSet =>
        reg | (newVal << bitPos)
      case WriteAction.OneToToggle =>
        reg ^ (newVal << bitPos)
      case WriteAction.ClearOnRead =>
        reg
    }
  }
}
