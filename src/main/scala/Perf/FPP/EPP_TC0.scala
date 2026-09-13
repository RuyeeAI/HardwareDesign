package Perf.FPP

import Perf.common.{PacketDistribution, PacketGeneratorPara}

class EPP_TC0{
  val c = EppDatapathCfg(
    eppPacketGeneratorCfg = EppPacketGeneratorCfg(
      portCfg = Array(
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(9600,9600)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 100, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(9600,9600)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),
        PacketGeneratorPara(Bandwidth = 0, bucketDep = 1000, dist = PacketDistribution("Random",sizeRange = Array(408,408)) , packetNum = 100,  initCreditCounter = 1000, creditRes = 8, creditPreDec = 0),

      ),
      schTdmCal = Array(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15),
      schWrrWeightCfg = Array(8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8),
      schWrrMaxWeight = 11
    ) ,
    eppLatency = 101
  )

  val epp_dp = new EppDatapath(c)


}
