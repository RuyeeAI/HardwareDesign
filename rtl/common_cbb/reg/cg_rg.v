module cg_rg(
    input wire                  i_clk,
    input wire                  i_rst_n,

    input wire                  i_dat_vld,
    input wire [DATA_W-1:0]     i_dat,
    
    output reg [DATA_W-1:0]     o_dat
);

always@(posedge i_clk)begin
    if(!i_rst_n)
        o_dat <= {DATA_W{1'b0}};
    else if(i_dat_vld==1'b1)
        o_dat <= i_dat;
    else;
end

endmodule
