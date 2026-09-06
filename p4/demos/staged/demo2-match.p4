// M2 demo 的切拍变体（staged 目录变体允许与主目录源程序不同）：
// action 含 2 级计算链（slice + add），表匹配整体原子（key/hit 第 0 级组合），
// action 部分切拍：预算 P4C_STAGED_STAGES（默认 4）→ 实际级数 n = min(4, D+1) = 3。
// 供 Demo4StagesSpec 中的 Demo2MatchStagedIngress 等价性用例使用。
#include <core.p4>

header ethernet_h {
    bit<48> dstAddr;
    bit<48> srcAddr;
    bit<16> etherType;
}

struct headers_t {
    ethernet_h ethernet;
}

struct metadata_t {
    bit<16> normPort;
    bit<8>  cls;
}

control Ingress(inout headers_t hdr, inout metadata_t meta) {
    action set_cls(bit<8> c) {
        meta.cls = c + meta.normPort[7:0];
        meta.normPort = 16w0;
    }
    action nop() { }

    table cls_table {
        key = {
            hdr.ethernet.etherType : exact;
        }
        actions = {
            set_cls;
            nop;
        }
        const entries = {
            0x0800 : set_cls(8w7);
            0x86dd : set_cls(8w9);
            default : nop();
        }
    }

    apply {
        cls_table.apply();
    }
}
