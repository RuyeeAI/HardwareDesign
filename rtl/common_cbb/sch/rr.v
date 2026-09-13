///============================================================================
/// FILE NAME    :   rr.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-9-27 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose : round robin scheduler.                                           
///============================================================================

module rr#(
    parameter PORT_NUM = 4
)(
    input                   i_clk,
    input                   i_rst_n,
    input                   i_schedule_en,
    input   [PORT_NUM -1:0] i_rdy,
    output  [PORT_NUM -1:0] o_grant,
    output                  o_grant_vld
);
reg [PORT_NUM -1:0]     pointer_ff;
wire[PORT_NUM -1:0]     un_mask_req;
wire[PORT_NUM -1:0]     un_mask_pe;
wire[PORT_NUM -1:0]     mask_req;
wire[PORT_NUM -1:0]     mask_pe;
wire [PORT_NUM -1:0]    mask_grant;
wire [PORT_NUM -1:0]    un_mask_grant;


assign mask_req = i_rdy & pointer_ff;
assign un_mask_req = i_rdy;
generate
genvar i;
for(i=0;i<PORT_NUM;i=i+1)begin:EACH_BIT
    if(i==0)begin
        assign mask_pe[i] = 1'b0;
        assign un_mask_pe[i] = 1'b0;
    end
    else begin
        assign mask_pe[i] = |mask_req[i-1:0];
        assign un_mask_pe[i] = |un_mask_req[i-1:0];
    end
end
endgenerate

assign mask_grant = ~mask_pe & mask_req;
assign un_mask_grant = ~un_mask_pe & un_mask_req;
assign o_grant = |mask_req ? mask_grant : un_mask_grant;
assign o_grant_vld = i_schedule_en & |i_rdy;

always@(posedge i_clk)begin
    if(i_rst_n==1'b0)
        pointer_ff <= {{(PORT_NUM-1){1'b0}},1'b1};
    else if(|mask_req & i_schedule_en)
        pointer_ff <= mask_pe;
    else if(|un_mask_req & i_schedule_en)
        pointer_ff <= un_mask_pe;
    else ;
end

endmodule
        