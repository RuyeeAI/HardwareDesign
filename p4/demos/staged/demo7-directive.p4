// E2 demo：声明级切拍编译指示（// p4c: stages=N）。
// control Fast 声明上方紧邻 `// p4c: stages=2` → 无论全局预算如何，Fast 恒切 2 级；
// control Slow 无指示 → 走全局预算（staged 管线默认 P4C_STAGED_STAGES=4）。
// 两个 control 计算同一函数（f0..f7 求和），供 Demo7DirectiveSpec 做跨拍数等价比较。
#include <core.p4>

struct metadata_t {
    bit<16> f0;
    bit<16> f1;
    bit<16> f2;
    bit<16> f3;
    bit<16> f4;
    bit<16> f5;
    bit<16> f6;
    bit<16> f7;
    bit<16> acc;
}

// p4c: stages=2
control Fast(inout metadata_t meta) {
    action chain() {
        // 8 项加法链：Bin 链加权深度 W=7，指示 2 级 → n = min(2, W+1) = 2
        meta.acc = meta.f0 + meta.f1 + meta.f2 + meta.f3
                 + meta.f4 + meta.f5 + meta.f6 + meta.f7;
    }

    apply { chain(); }
}

control Slow(inout metadata_t meta) {
    action chain() {
        // 同一函数：无指示，全局预算（默认 4）→ n = min(4, W+1) = 4
        meta.acc = meta.f0 + meta.f1 + meta.f2 + meta.f3
                 + meta.f4 + meta.f5 + meta.f6 + meta.f7;
    }

    apply { chain(); }
}
