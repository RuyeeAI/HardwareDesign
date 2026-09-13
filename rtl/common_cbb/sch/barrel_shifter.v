/****************************************************************************
 * barrel_shifter.v
 ****************************************************************************/

/**
 * Class: barrel_shifter
 * 
 * TODO: Add class documentation
 */
module barrel_shifter#(
	parameter DAT_W 	= 32,
	parameter SFT_LEFT 	= 1,
	parameter SFT_W 	= log2(SRC_DAT_W)
)(
	input	[DAT_W -1:0]		i_src_dat,
	input	[SFT_W -1:0]		i_sft_bit,
	output  [DAT_W -1:0]		o_dst_dat
);
	
reg	[DAT_W -1:0]				stg_dat[SFT_W:0];
assign stg_dat[0] = i_src_dat;
generate 
genvar i;
genvar j;
	for(i=1;i<=SFT_W;i=i+1)begin:GEN_EACH_STG
		for(j=0;j<DST_W;j=j+1)begin:GEN_EACH_BIT
			always@(*)begin
				stg_dat[i][j] = i_sft_bit[i] ? stg_dat[i-1][j-2**(i-1)]:
	
	
	
	
	
	
	
endmodule