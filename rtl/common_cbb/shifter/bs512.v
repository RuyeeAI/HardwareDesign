module bs512(
    input   [512 -1:0]                 i_src_dat,
    input   [9 -1:0]           i_shift_bit,
    output  [512 -1:0]                 o_dst_dat
);

wire   [512 -1:0]                 lvl0_dat;
wire   [512 -1:0]                 lvl1_dat;
wire   [512 -1:0]                 lvl2_dat;
wire   [512 -1:0]                 lvl3_dat;
wire   [512 -1:0]                 lvl4_dat;
wire   [512 -1:0]                 lvl5_dat;
wire   [512 -1:0]                 lvl6_dat;
wire   [512 -1:0]                 lvl7_dat;
wire   [512 -1:0]                 lvl8_dat;
wire   [512 -1:0]                 lvl9_dat;
assign lvl0_dat = i_src_dat;
assign lvl1_dat = i_shift_bit[0] ? {lvl0_dat[511 -1:0],1'b0} : lvl0_dat;
assign lvl2_dat = i_shift_bit[1] ? {lvl1_dat[510 -1:0],2'b0} : lvl1_dat;
assign lvl3_dat = i_shift_bit[2] ? {lvl2_dat[508 -1:0],4'b0} : lvl2_dat;
assign lvl4_dat = i_shift_bit[3] ? {lvl3_dat[504 -1:0],8'b0} : lvl3_dat;
assign lvl5_dat = i_shift_bit[4] ? {lvl4_dat[496 -1:0],16'b0} : lvl4_dat;
assign lvl6_dat = i_shift_bit[5] ? {lvl5_dat[480 -1:0],32'b0} : lvl5_dat;
assign lvl7_dat = i_shift_bit[6] ? {lvl6_dat[448 -1:0],64'b0} : lvl6_dat;
assign lvl8_dat = i_shift_bit[7] ? {lvl7_dat[384 -1:0],128'b0} : lvl7_dat;
assign lvl9_dat = i_shift_bit[8] ? {lvl8_dat[256 -1:0],256'b0} : lvl8_dat;
assign o_dst_dat = lvl9_dat;
endmodule
