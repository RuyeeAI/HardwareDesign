module bs16(
    input   [16 -1:0]                 i_src_dat,
    input   [4 -1:0]           i_shift_bit,
    output  [16 -1:0]                 o_dst_dat
);

wire   [16 -1:0]                 lvl0_dat;
wire   [16 -1:0]                 lvl1_dat;
wire   [16 -1:0]                 lvl2_dat;
wire   [16 -1:0]                 lvl3_dat;
wire   [16 -1:0]                 lvl4_dat;
assign lvl0_dat = i_src_dat;
assign lvl1_dat = i_shift_bit[0] ? {lvl0_dat[15 -1:0],1'b0} : lvl0_dat;
assign lvl2_dat = i_shift_bit[1] ? {lvl1_dat[14 -1:0],2'b0} : lvl1_dat;
assign lvl3_dat = i_shift_bit[2] ? {lvl2_dat[12 -1:0],4'b0} : lvl2_dat;
assign lvl4_dat = i_shift_bit[3] ? {lvl3_dat[8 -1:0],8'b0} : lvl3_dat;
assign o_dst_dat = lvl4_dat;
endmodule
