package BaseCbb.RegCbb.dsl

/** Field allocation within a register */
case class FieldAllocation(
  field: RegFieldDef,
  byteOffset: BigInt,
  bitOffset: Int,
  register: RegDef
)

/** Register allocation with all its fields */
case class RegisterAllocation(
  register: RegDef,
  byteOffset: BigInt,
  byteSize: Int,
  wordOffset: Int,      // 起始32bit word索引 (addr[11:2])
  wordCount: Int,       // 占用的32bit word数量
  fieldAllocations: Seq[FieldAllocation]
)

/** Memory allocation */
case class MemoryAllocation(
  memory: MemoryDef,
  baseAddress: BigInt,
  endAddress: BigInt
)

/** Complete address map for a register block */
case class AddressMap(
  block: RegBlockDef,
  registerAllocations: Seq[RegisterAllocation],
  memoryAllocations: Seq[MemoryAllocation],
  totalRegByteSize: BigInt,
  totalMemByteSize: BigInt
)

/** Address allocator for register blocks with dual address spaces */
object AddressAllocator {
  /** Allocate addresses for all registers and memories in a block */
  def allocate(block: RegBlockDef): AddressMap = {
    // Allocate register addresses
    var currentRegOffset = 0
    val regAllocations = block.registers.map { reg =>
      val totalBits = reg.fields.map(_.bitWidth).sum
      val byteSize = (totalBits + 7) / 8

      // Align to 4 bytes for multi-word registers
      val alignedOffset = if (byteSize > 4) {
        (currentRegOffset + 3) & ~3
      } else {
        currentRegOffset
      }

      // Calculate word-based addressing (each word = 32 bits)
      val wordOffset = ((block.regBaseAddress + alignedOffset) / 4).toInt
      val wordCount = (byteSize + 3) / 4

      // Allocate field positions within register (LSB-first)
      var currentBit = 0
      val fieldAllocations = reg.fields.map { field =>
        val alloc = FieldAllocation(
          field = field,
          byteOffset = alignedOffset + (currentBit / 8),
          bitOffset = currentBit % 8,
          register = reg
        )
        currentBit += field.bitWidth
        alloc
      }

      currentRegOffset = alignedOffset + byteSize
      RegisterAllocation(reg, alignedOffset, byteSize, wordOffset, wordCount, fieldAllocations)
    }

    // Allocate memory addresses (each memory has its own space)
    val memAllocations = block.memories.map { mem =>
      MemoryAllocation(
        memory = mem,
        baseAddress = mem.baseAddress,
        endAddress = mem.baseAddress + mem.byteSize - 1
      )
    }

    // Calculate total sizes
    val totalRegSize: BigInt = if (regAllocations.isEmpty) BigInt(0) else
      regAllocations.map(r => r.byteOffset + r.byteSize).max + 1

    val totalMemSize: BigInt = if (memAllocations.isEmpty) BigInt(0) else
      memAllocations.map(m => m.endAddress + 1).max

    AddressMap(block, regAllocations, memAllocations, totalRegSize, totalMemSize)
  }

  /** Create a summary of register layout for debugging */
  def summarize(map: AddressMap): String = {
    val sb = new StringBuilder
    sb ++= s"Register Block: ${map.block.name}\n"
    sb ++= s"Device: ${map.block.deviceName}\n"
    sb ++= s"Register Base Address: 0x${map.block.regBaseAddress.toString(16)}\n"
    sb ++= s"Memory Base Address: 0x${map.block.memBaseAddress.toString(16)}\n"
    sb ++= s"Total Register Space: ${map.totalRegByteSize} bytes\n"
    sb ++= s"Total Memory Space: ${map.totalMemByteSize} bytes\n\n"

    sb ++= "=== Registers ===\n"
    map.registerAllocations.foreach { regAlloc =>
      sb ++= s"  ${regAlloc.register.name} @ offset 0x${regAlloc.byteOffset.toString(16)} (${regAlloc.byteSize} bytes, word ${regAlloc.wordOffset}, ${regAlloc.wordCount} words)\n"
      regAlloc.fieldAllocations.foreach { fieldAlloc =>
        val field = fieldAlloc.field
        sb ++= s"    Field: ${field.name} [${field.bitWidth}b] @ byte ${fieldAlloc.byteOffset}, bit ${fieldAlloc.bitOffset}\n"
        sb ++= s"      Access: ${field.access}, Reset: ${field.resetValue}\n"
        if (field.description.nonEmpty) {
          sb ++= s"      Desc: ${field.description}\n"
        }
      }
    }

    if (map.memoryAllocations.nonEmpty) {
      sb ++= "\n=== Memories ===\n"
      map.memoryAllocations.foreach { memAlloc =>
        sb ++= s"  ${memAlloc.memory.name} @ 0x${memAlloc.baseAddress.toString(16)}\n"
        sb ++= s"    Depth: ${memAlloc.memory.depth}, Width: ${memAlloc.memory.dataWidth}b\n"
        sb ++= s"    Type: ${memAlloc.memory.memType}, Size: ${memAlloc.memory.byteSize} bytes\n"
      }
    }

    sb.toString
  }
}
