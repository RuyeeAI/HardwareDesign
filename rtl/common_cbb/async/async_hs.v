///============================================================================
/// FILE NAME    :   async_hs.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-3 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  synchronizer for a pulse.                                           
///============================================================================

module async_hs#(
    parameter SYNC2     = 0
)(
    input                           i_a_clk,
    input                           i_a_rst_n,
    input                           i_b_clk,
    input                           i_b_rst_n,
    input                           i_a_pulse,
    output                          o_b_pulse
);

reg                                 req;
reg                                 a_pulse_ff;
wire                                pulse_a;
wire                                req_a2b;
wire                                ack;
wire                                ack_b2a;
reg                                 ack_ff;

always@(posedge i_a_clk)begin
    if(!i_a_rst_n)
        a_pulse_ff <= 1'b0;
    else    
        a_pulse_ff <= i_a_pulse;
end

assign pulse_a = i_a_pulse & !a_pulse_ff;


always@(posedge i_a_clk)begin
    if(!i_a_rst_n)
        req <= 1'b0;
    else if(!req & pulse_a)
        req <= 1'b1;
    else if(req & ack_b2a)
        req <= 1'b0;
    else ;
end

synchronizer #(
    .BUS_WIDTH   (1           ),
    .SYNC2       (SYNC2       )
)U_REQ_A2B_SYNC(
    .i_dst_clk   (i_b_clk     ),
    .i_dat_in    (req         ),
    .o_dat_out   (req_a2b     )
);

assign ack = req_a2b;
synchronizer #(
    .BUS_WIDTH   (1           ),
    .SYNC2       (SYNC2       )
)U_ACK_B2A_SYNC(
    .i_dst_clk   (i_a_clk     ),
    .i_dat_in    (ack         ),
    .o_dat_out   (ack_b2a     )
);


always@(posedge i_b_clk)begin
    if(!i_b_rst_n)
        ack_ff <= 1'b0;
    else 
        ack_ff <= ack;
end

assign o_b_pulse = !ack_ff & ack;

endmodule


    
    
    