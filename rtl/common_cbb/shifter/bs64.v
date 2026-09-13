module bs64(
    input   [64 -1:0]                 i_src_dat,
    input   [6 -1:0]           i_shift_bit,
    output  [64 -1:0]                 o_dst_dat
);

wire   [64 -1:0]                 lvl0_dat;
wire   [64 -1:0]                 lvl1_dat;
wire   [64 -1:0]                 lvl2_dat;
wire   [64 -1:0]                 lvl3_dat;
wire   [64 -1:0]                 lvl4_dat;
wire   [64 -1:0]                 lvl5_dat;
wire   [64 -1:0]                 lvl6_dat;
assign lvl0_dat = i_src_dat;
assign lvl1_dat = i_shift_bit[0] ? {lvl0_dat[63 -1:0],1'b0} : lvl0_dat;
assign lvl2_dat = i_shift_bit[1] ? {lvl1_dat[62 -1:0],2'b0} : lvl1_dat;
assign lvl3_dat = i_shift_bit[2] ? {lvl2_dat[60 -1:0],4'b0} : lvl2_dat;
assign lvl4_dat = i_shift_bit[3] ? {lvl3_dat[56 -1:0],8'b0} : lvl3_dat;
assign lvl5_dat = i_shift_bit[4] ? {lvl4_dat[48 -1:0],16'b0} : lvl4_dat;
assign lvl6_dat = i_shift_bit[5] ? {lvl5_dat[32 -1:0],32'b0} : lvl5_dat;
assign o_dst_dat = lvl6_dat;
endmodule
