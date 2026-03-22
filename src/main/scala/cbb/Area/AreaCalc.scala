package BaseCbb.Area


object ProcessConfiguration{
  val process:String = "T7"

  var mux2_area = 0.15
  var ff_area   = 0.44
  var nd2_area  = 0.048

  if(process =="T7"){
    mux2_area = 0.15
    ff_area   = 0.36
    nd2_area  = 0.0547
  }else if(process == "S12"){
    mux2_area = 0.3
    ff_area   = 1
    nd2_area  = 0.111
  }else if(process == "S7"){
    mux2_area = 0.15
    ff_area   = 0.44
    nd2_area  = 0.048
  }

  val logic_uti = 0.4
  val xbar_uti  = 0.3
  val mem_uti  = 0.75
  val tcam_uti = 0.6
  val comb_incr_syn2pr = 1.4

  val pd_mux2_area = mux2_area*comb_incr_syn2pr/xbar_uti
  val pd_ff_area   = ff_area/logic_uti
  val pd_nd2_area  = nd2_area*comb_incr_syn2pr/logic_uti
}

object IR2AreaCommon{

}

case class GenArea(ff_num:Int, comb_area:Double=0, mem_area:Double=0){
  val ff_area:Double = ff_num*ProcessConfiguration.pd_ff_area
  val total_area: Double = ff_area + mem_area + comb_area
}
