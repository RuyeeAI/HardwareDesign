package FPP.FvProfile

import scala.collection.mutable.ArrayBuffer

abstract class AbsFvStruct{
  /**
   * Map from fv offset to valid index, when valid index is -1, it means the fv is always valid.
   */
  var bits:Map[Int,Int]
  /**
   * the proc per profile to each bit
   */
  var procPerProfile:Map[Int,Array[String]]

  def SplitSubGroup(bits:Map[Int,Int],procPerProfile:Map[Int,Array[String]],benchmarkProfile:Int)={
    var subgroup:Map[String,ArrayBuffer[Int]] = Map()
    for(b<- bits.keys){
      /**
       * Take 2 profiles as example, there all following cases:
       * the subgroup at input is split into 4 sub groups:
       * 1. WR, WR        -- move all always valid sub-group, this will be merged into always valid group.
       * 2. WR, NA/RD     -- move to profile0-only sub-group, valid = valid && profile ==0
       * 3. NA/RD, WR     -- move to profile1-only sub-group, valid = valid && profile ==1
       * 4. NA/RD, NA/RD  -- move to original valid sub-group, valid = valid
       *
       * When there're more than 2 profiles, for example, there're 3 profiles:
       * There're following case:
       * 1. WR,WR,WR
       * 2. WR,NR,NR,
       * 3. NR,WR,WR
       * 4. NR,NR,WR
       * 5. WR,WR,NR,
       * 6. WR,NR,WR,
       * 7. NR,WR,WR
       * 8. NR,NR,NR
       */
      val proc = procPerProfile(b)
      val subGroupName = bits(b).toString+"_"+proc.mkString("_")
      val a:ArrayBuffer[Int] = ArrayBuffer()
      if(subgroup.contains(subGroupName)){
        val a = subgroup(subGroupName)
      }
      a+=b
      subgroup+=(subGroupName->a)
    }

    // This is initial sub groups
    MergeSubGroup(subgroup)
    // Merge
    subgroup
  }

  def MergeSubGroup(isg:Map[String,ArrayBuffer[Int]]) = {

  }


}

//
//class FvProfiles {
//  var procPerProfile:Map[Int,Array[String]]
//}
