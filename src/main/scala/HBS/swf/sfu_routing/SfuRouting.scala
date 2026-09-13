package HBS.swf.sfu_routing

import BaseCbb.data.GenModule
import BaseCbb.misc.LatencyPipeV
import HBS.swf.common._
import chisel3._
import chisel3.util._

import scala.collection.mutable.ArrayBuffer

class SfuRouting(p: SwfParams) extends GenModule {
  val ib_swf_intf         = IO(Vec(2,new IbSwfIntf(p)))
  val dsc_swf_intf        = IO(new DscSwfIntf(p))
  val rout_up_cor_intf    = IO(Flipped(new RoutSfuIntfBundle(p)))
  val rout_up_mid_intf    = IO(Flipped(new RoutSfuIntfBundle(p)))
  val rout_dn_cor_intf    = IO(Flipped(new RoutSfuIntfBundle(p)))
  val rout_dn_mid_intf    = IO(Flipped(new RoutSfuIntfBundle(p)))

  // Calculate the sampling number per bus
  val LatencyPara = LatencyCalculation

  for(ib<-0 until 2) {
    for (i <- 0 until p.IbSwfDp) yield {
      val U_DP = Module(new SfuDatapath(UInt((new InterfaceISReq(p)).getWidth.W), LatencyPara(i)))
      U_DP.io.ib_in.bits := ib_swf_intf(ib).ib_swf_data(i).bits.asUInt
      U_DP.io.ib_in.valid := ib_swf_intf(ib).ib_swf_data(i).valid
      if (ib==0) {
        rout_up_cor_intf.data(i).req.data := U_DP.io.up_out
        rout_dn_cor_intf.data(i).req.data := U_DP.io.dn_out
      } else {
        rout_up_mid_intf.data(i ).req.data := U_DP.io.up_out
        rout_dn_mid_intf.data(i ).req.data := U_DP.io.dn_out
      }
    }


    // Ack, assume only take one cycle to IB controller.
    if(ib==0){
      for (i <- 0 until p.RoutSfuSwiDp) {
        ib_swf_intf(ib).ib_swf_data_ack(i)                      := LatencyPipeV(rout_up_cor_intf.data(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_data_ack(i).cloneType)
        ib_swf_intf(ib).ib_swf_data_ack(i + p.RoutSfuSwiDp * 1) := LatencyPipeV(rout_dn_cor_intf.data(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_data_ack(i).cloneType)
      }
    }else{
      for (i <- 0 until p.RoutSfuSwiDp) {
        ib_swf_intf(ib).ib_swf_data_ack(i )     := LatencyPipeV(rout_up_mid_intf.data(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_data_ack(i).cloneType)
        ib_swf_intf(ib).ib_swf_data_ack(i + p.RoutSfuSwiDp * 1) := LatencyPipeV(rout_dn_mid_intf.data(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_data_ack(i).cloneType)
      }

    }

    for (i <- 0 until p.IbSwfDsc) yield {
      val U_DP = Module(new SfuDatapath(UInt((new InterfaceIDReq(p)).getWidth.W), LatencyPara(i / 3)))
      U_DP.io.ib_in := ib_swf_intf(ib).ib_swf_ct_dsc(i).asTypeOf(U_DP.io.ib_in.cloneType)
      if (i < p.RoutSfuSwiCtCp) {
        rout_up_cor_intf.ct_dsc(i).req.data := U_DP.io.up_out
        rout_dn_cor_intf.ct_dsc(i).req.data := U_DP.io.dn_out
      } else {
        rout_up_mid_intf.ct_dsc(i - p.RoutSfuSwiCtCp).req.data := U_DP.io.up_out
        rout_dn_mid_intf.ct_dsc(i - p.RoutSfuSwiCtCp).req.data := U_DP.io.dn_out
      }
    }

    for (i <- 0 until p.DscSwfDsc) yield {
      val U_DP = Module(new SfuDatapath(UInt((new InterfaceIDReq(p)).getWidth.W), LatencyPara(i / 2)))
      U_DP.io.ib_in := dsc_swf_intf.saf_dsc(i).asTypeOf(U_DP.io.ib_in.cloneType)
      if (i < p.RoutSfuSwiSafCp) {
        rout_up_cor_intf.saf_dsc(i).req.data := U_DP.io.up_out
        rout_dn_cor_intf.saf_dsc(i).req.data := U_DP.io.dn_out
      } else {
        rout_up_mid_intf.saf_dsc(i - p.RoutSfuSwiSafCp).req.data := U_DP.io.up_out
        rout_dn_mid_intf.saf_dsc(i - p.RoutSfuSwiSafCp).req.data := U_DP.io.dn_out
      }
    }

    for (i <- 0 until p.RoutSfuSwiCtCp) {
      ib_swf_intf(ib).ib_swf_ct_dsc_ack(i)                        := LatencyPipeV(rout_up_cor_intf.ct_dsc(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_ct_dsc_ack(i).cloneType)
      ib_swf_intf(ib).ib_swf_ct_dsc_ack(i + p.RoutSfuSwiCtCp)     := LatencyPipeV(rout_up_mid_intf.ct_dsc(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_ct_dsc_ack(i).cloneType)
      ib_swf_intf(ib).ib_swf_ct_dsc_ack(i + p.RoutSfuSwiCtCp * 2) := LatencyPipeV(rout_dn_cor_intf.ct_dsc(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_ct_dsc_ack(i).cloneType)
      ib_swf_intf(ib).ib_swf_ct_dsc_ack(i + p.RoutSfuSwiCtCp * 3) := LatencyPipeV(rout_dn_mid_intf.ct_dsc(i).ack, 1).asTypeOf(ib_swf_intf(ib).ib_swf_ct_dsc_ack(i).cloneType)
    }

    for (i <- 0 until p.RoutSfuSwiSafCp) {
      dsc_swf_intf.saf_dsc_ack(i)                          := LatencyPipeV(rout_up_cor_intf.saf_dsc(i).ack, 1).asTypeOf(dsc_swf_intf.saf_dsc_ack(i).cloneType)
      dsc_swf_intf.saf_dsc_ack(i + p.RoutSfuSwiSafCp)      := LatencyPipeV(rout_up_mid_intf.saf_dsc(i).ack, 1).asTypeOf(dsc_swf_intf.saf_dsc_ack(i).cloneType)
      dsc_swf_intf.saf_dsc_ack(i + p.RoutSfuSwiSafCp * 2)  := LatencyPipeV(rout_dn_cor_intf.saf_dsc(i).ack, 1).asTypeOf(dsc_swf_intf.saf_dsc_ack(i).cloneType)
      dsc_swf_intf.saf_dsc_ack(i + p.RoutSfuSwiSafCp * 3)  := LatencyPipeV(rout_dn_mid_intf.saf_dsc(i).ack, 1).asTypeOf(dsc_swf_intf.saf_dsc_ack(i).cloneType)
    }
  }
  /**
   * Calcualte the sampling number in SFU Routing.
   *
   * @return sampling number to upper SFUs and bottom SFUs.
   */
  def LatencyCalculation: ArrayBuffer[Seq[Int]] = {
    // Calculate the sampling number per bus
    val PerCycleDistance = 0.7
    val WireDensity = 30000
    val SfuRoutingHeight = p.IbSwfDp * 2048 * 1.1 / WireDensity
    val totalSampling = math.ceil(SfuRoutingHeight * (p.IbSwfDp + 1) / p.IbSwfDp / PerCycleDistance).toInt
    var Sampling: ArrayBuffer[Seq[Int]] = ArrayBuffer()
    //println(SfuRoutingHeight)

    for (i <- 0 until p.IbSwfDp) {
      val in_sample = math.ceil(SfuRoutingHeight * (i + 1) / p.IbSwfDp / PerCycleDistance).toInt
      val dn_sample = if ((totalSampling - in_sample) == 0) 1 else (totalSampling - in_sample)

      Sampling.append(Seq(in_sample, in_sample, dn_sample))
    }
    //println(Sampling)

    Sampling
  }

}

