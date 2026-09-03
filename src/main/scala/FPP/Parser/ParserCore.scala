package FPP.Parser

import BaseCbb.data.GenModule
import chisel3._
import chisel3.util._

// 协议解析函数见 [[ParserFunctions]]，类型定义见 [[ParserTypes]]，
// 流水线配置见 [[ParserPipeline]]。

// ============= Main Parser Core =============
class ParserCore(
  val pipelineConfig: ParserPipelineConfig = ParserPipelineConfig()
) extends GenModule {
  val io = IO(new Bundle {
    val in = Flipped(DecoupledIO(UInt(512.W)))
    val out = ValidIO(new ParseResult)
    val parseDone = Output(Bool())
    val meta = Output(new ParseMeta)
  })

  // Parser state machine
  val state = RegInit(ParserState.Idle)
  val nextState = Wire(chiselTypeOf(state))

  // Working registers
  val workBytes = Reg(UInt(512.W))
  val workMeta = Reg(new ParseMeta)
  val workNextType = Reg(UInt(8.W))
  val workParsedBytes = Reg(UInt(16.W))

  // Header tracking registers (PHO/PHI)
  val headerOffsets = Reg(Vec(24, UInt(16.W)))
  val headerDescs = Reg(Vec(24, new PacketHeaderDesc))
  val headerCount = Reg(UInt(5.W))

  // 流水线握手：本级已发射但结果尚未回收。等待期间不得重复解析 / 重复记录 PHO。
  val pipeIssued = RegInit(false.B)

  // Output bundle (combinational -- the registers below are updated on the same
  // cycle as io.out.valid, so a registered result would lag one packet behind).
  val outBits = Wire(new ParseResult)
  outBits := 0.U.asTypeOf(new ParseResult)

  // Default outputs
  io.out.valid := false.B
  io.out.bits := outBits
  io.parseDone := false.B
  io.meta := workMeta

  /** 跳过刚解析完的 `by` 字节（字节 0 在最高位段，故左移把下一层头部顶到最高位）。 */
  def shiftBytes(bytes: UInt, by: UInt): UInt = bytes << (by * 8.U)

  // Helper: convert Valid to Decoupled for pipeline stage
  def validToDecoupled[T <: Data](in: ValidIO[T]): DecoupledIO[T] = {
    val d = Wire(DecoupledIO(chiselTypeOf(in.bits)))
    d.valid := in.valid
    d.bits := in.bits
    d.ready := false.B
    d
  }

  // Helper: create pipeline stage if enabled
  def pipeAfter[T <: Data](data: ValidIO[T], enable: Boolean): ValidIO[T] = {
    val dec = validToDecoupled(data)
    val piped = PipelineStage(dec, enable)
    val result = Wire(Valid(chiselTypeOf(data.bits)))
    result.valid := piped.valid
    result.bits := piped.bits
    result
  }

  // Helper: record header in PHO/PHI arrays
  def recordHeader(offset: UInt, hdrType: UInt, length: UInt, valid: Bool, errorCode: UInt) = {
    when(headerCount < 24.U) {
      headerOffsets(headerCount) := offset
      headerDescs(headerCount).headerType := hdrType
      headerDescs(headerCount).offset := offset
      headerDescs(headerCount).length := length
      headerDescs(headerCount).valid := valid
      headerDescs(headerCount).errorCode := errorCode
      headerCount := headerCount + 1.U
    }
  }

  /**
   * 发射一级解析结果进入（可选的）级间流水线，并在结果回收时提交到工作寄存器。
   *
   * @return (out, advance) —— `advance` 为真的那一拍 FSM 才能前进
   */
  def issueStage(
      bytesNext: UInt,
      metaNext: ParseMeta,
      nextTypeNext: UInt,
      parsedNext: UInt,
      ok: Bool,
      pipeEnable: Boolean
  ): (ValidIO[InterstageData], Bool) = {
    val in = Wire(Valid(new InterstageData))
    in.valid := !pipeIssued && ok
    in.bits.bytes := bytesNext
    in.bits.meta := metaNext
    in.bits.nextType := nextTypeNext
    in.bits.parsedBytes := parsedNext
    in.bits.valid := ok

    val out = pipeAfter(in, pipeEnable)
    // 解析出错时结果不会进入流水线，但错误标志必须落到 workMeta 上，
    // 否则 io.meta / out.bits.valid 反映不出失败原因。
    when(!ok) {
      workMeta := metaNext
    }
    when(out.valid) {
      workBytes := out.bits.bytes
      workMeta := out.bits.meta
      workNextType := out.bits.nextType
      workParsedBytes := out.bits.parsedBytes
      pipeIssued := false.B
    }.elsewhen(in.valid) {
      pipeIssued := true.B
    }
    (out, out.valid)
  }

  /** Fill the result bundle from the current working set. */
  def emitResult(valid: Bool): Unit = {
    outBits.fields := workBytes
    outBits.nextType := workNextType
    outBits.headerLen := workParsedBytes
    outBits.valid := valid
    outBits.meta := workMeta
    outBits.headerCount := headerCount
    for (i <- 0 until 24) {
      outBits.pho(i) := headerOffsets(i)
      outBits.phi(i) := headerDescs(i)
    }
    io.out.valid := true.B
    io.parseDone := true.B
  }

  // Compute next state based on current state
  nextState := state
  switch(state) {
    is(ParserState.Idle) {
      when(io.in.valid) {
        workBytes := io.in.bits
        workMeta := 0.U.asTypeOf(new ParseMeta)
        workMeta.totalLen := 512.U
        workMeta.checksumValid := true.B
        workParsedBytes := 0.U
        workNextType := HeaderType.UNKNOWN
        headerCount := 0.U
        pipeIssued := false.B
        nextState := ParserState.Eth
      }
    }

    is(ParserState.Eth) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseEthernet(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterEth
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        // workBytes 的更新要到下一拍才生效，这里读到的仍是本层头部
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.IPV4 -> ParserState.Ipv4,
            HeaderType.IPV6 -> ParserState.Ipv6,
            HeaderType.ARP  -> ParserState.Arp,
            HeaderType.VLAN -> ParserState.Vlan,
            HeaderType.MPLS -> ParserState.Mpls
          )
        )
      }
    }

    is(ParserState.Vlan) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseVlan(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterVlan
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.IPV4 -> ParserState.Ipv4,
            HeaderType.IPV6 -> ParserState.Ipv6,
            HeaderType.ARP  -> ParserState.Arp,
            HeaderType.VLAN -> ParserState.Vlan,
            HeaderType.MPLS -> ParserState.Mpls
          )
        )
      }
    }

    is(ParserState.Mpls) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseMpls(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterMpls
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        // workBytes 要到下一拍才更新，下一层头部的首字节在 out.bits.bytes 里
        when(out.bits.nextType === HeaderType.MPLS) {
          nextState := ParserState.Mpls
        }.otherwise {
          // 栈底之后的猜测：用 IP 版本号区分 v4 / v6，否则当纯载荷
          val ver = out.bits.bytes(511, 508)
          nextState := Mux(ver === 4.U, ParserState.Ipv4,
                       Mux(ver === 6.U, ParserState.Ipv6,
                         ParserState.Payload))
        }
      }
    }

    is(ParserState.Ipv4) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIpv4(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterIpv4
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.TCP  -> ParserState.Tcp,
            HeaderType.UDP  -> ParserState.Udp,
            HeaderType.ICMP -> ParserState.Icmp,
            HeaderType.GRE  -> ParserState.TunnelGre,
            HeaderType.MPLS -> ParserState.Mpls
          )
        )
      }
    }

    is(ParserState.Ipv6) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIpv6(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterIpv6
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Payload)(
          Seq(
            HeaderType.TCP  -> ParserState.Tcp,
            HeaderType.UDP  -> ParserState.Udp,
            HeaderType.ICMP -> ParserState.Icmp,
            HeaderType.GRE  -> ParserState.TunnelGre
          )
        )
      }
    }

    is(ParserState.Arp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseArp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterArp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Done
      }
    }

    is(ParserState.Tcp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseTcp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterTcp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Done
      }
    }

    is(ParserState.Udp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseUdp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterUdp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Done)(
          Seq(
            HeaderType.VXLAN  -> ParserState.TunnelVxlan,
            HeaderType.GENEVE -> ParserState.TunnelGeneve,
            HeaderType.GTPU   -> ParserState.TunnelGtpu
          )
        )
      }
    }

    is(ParserState.Icmp) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseIcmp(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterIcmp
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Done
      }
    }

    is(ParserState.TunnelVxlan) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseVxlan(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterVxlan
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Eth   // VXLAN 内层是完整以太网帧
      }
    }

    is(ParserState.TunnelGeneve) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGeneve(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterGeneve
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := ParserState.Eth   // Geneve 内层是完整以太网帧
      }
    }

    is(ParserState.TunnelGtpu) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGtpu(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterGtpu
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        // GTPU 内层是 IP，用版本号区分 v4 / v6（下一层头部在 out.bits.bytes）
        val ver = out.bits.bytes(511, 508)
        nextState := Mux(ver === 4.U, ParserState.Ipv4,
                     Mux(ver === 6.U, ParserState.Ipv6,
                       ParserState.Done))
      }
    }

    is(ParserState.TunnelGre) {
      val (_, nextType, hdrLen, newMeta, errorCode, hdrType) = parseGre(workBytes, workMeta)

      when(!pipeIssued) {
        recordHeader(workParsedBytes, hdrType, hdrLen, !newMeta.parseError, errorCode)
      }

      val (out, advance) = issueStage(
        shiftBytes(workBytes, hdrLen),
        newMeta,
        nextType,
        workParsedBytes + hdrLen,
        !newMeta.parseError,
        pipelineConfig.pipeAfterGre
      )

      when(newMeta.parseError) {
        nextState := ParserState.Error
      }.elsewhen(advance) {
        nextState := MuxLookup(out.bits.nextType, ParserState.Done)(
          Seq(
            HeaderType.IPV4 -> ParserState.Ipv4,
            HeaderType.IPV6 -> ParserState.Ipv6
          )
        )
      }
    }

    is(ParserState.Payload) {
      recordHeader(workParsedBytes, HeaderType.PAYLOAD, 0.U, true.B, HeaderErrorCode.None)
      nextState := ParserState.Done
    }

    is(ParserState.Done) {
      emitResult(!workMeta.parseError)
      headerCount := 0.U
      pipeIssued := false.B
      nextState := ParserState.Idle
    }

    is(ParserState.Error) {
      emitResult(false.B)
      headerCount := 0.U
      pipeIssued := false.B
      nextState := ParserState.Idle
    }
  }

  // Update state
  state := nextState

  // Input ready signal
  io.in.ready := (state === ParserState.Idle) ||
                 (state === ParserState.Done) ||
                 (state === ParserState.Error)
}

// ============= Companion Object =============

object ParserCore {
  def apply(): ParserCore = Module(new ParserCore(ParserPipelineConfig.default))
}
