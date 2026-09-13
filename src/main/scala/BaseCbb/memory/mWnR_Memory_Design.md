# Build mRnW Memory from SP memory

# Build TP from SP

假设原始需求为 D 深度的 1R1W memory， 需要使用：

* 3 块 D/2 深度的 SP memory, 多出的一块用于解决读写冲突问题
* 1 块 index memory 存储数据存储的位置

数据读出时首先读取index memory 判断读取的数据所在的位置，然后使用地址去所在的memory 读取
写数据时将数据在未被读取的两个 memory 中选一个写入，并将memory 的 index 记入 index memory

将上面的策略推广开，如果将 memory 拆分为 K+1 个bank,可以减少增加的 memory 开销。但是每个数据可能存储的位置有 K+1 个， K个数据共享 K+1 个位置，Index 的宽度增加


# Build 2R2W memory from SP
* 由于每个时钟周期有两个读操作，为避免两个读操作读冲突，则数据需要有两个备份保证两个读一定不冲突，每一次写操作需要同时写两块 memory
* 每个写操作需要同时写 2 个 memory， 共 4 块 memory
* 考虑读写操作冲突，共需 8个 memory
* 地址关联的 2 个数据共享 8 个可能的位置。INDEX 用于指示每个数据的 2 个存储位置。
  * 读数据时需要知道数据的存储位置；
  * 写数据时需要将数据放到未使用的位置
推广开，K 个数据共享 2*（K+2）个可能的位置（K=1，2，3，..) K越大，memory 使用量越少，INDEX 开销越大
  
# Build nRmW memory from SP
* K个数据共享 n*（K+m）个 memory