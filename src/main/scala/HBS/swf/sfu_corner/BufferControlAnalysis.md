# Options Comparsion
## Option1. use shared link list
* per source lane
  * one LL memory
  * one bitmap resource
  * (H+T)*RamLat*TargetNum
## Option2. use FIFOs
* per source lane
  * Ptr*FIfoDep*TargetNum
  * 