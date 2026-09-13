///============================================================================
/// FILE NAME    :   async_bus.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-2 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  synchronizer for a bus.                                           
///============================================================================

module async_bus#(
    parameter BUS_WIDTH = 16,
    parameter SYNC2     = 0
)(
    input  wire                     i_a_clk,
    input  wire                     i_a_rst_n,
    input  wire[BUS_WIDTH -1:0]     i_a_bus,
    input  wire                     i_b_clk,
    input  wire                     i_b_rst_n,
    output reg [BUS_WIDTH -1:0]     o_b_bus                   
);  

reg                             req;
reg                             ack;

wire                            req_a2b;
wire                            ack_b2a;
reg [BUS_WIDTH -1:0]            a_bus_ff;

synchronizer #(
    .BUS_WIDTH   (1           ),
    .SYNC2       (SYNC2       )
)U_REQ_A2B_SYNC(
    .i_dst_clk   (i_b_clk     ),
    .i_dat_in    (req         ),
    .o_dat_out   (req_a2b     )
);

synchronizer #(
    .BUS_WIDTH   (1           ),
    .SYNC2       (SYNC2       )
)U_ACK_B2A_SYNC(
    .i_dst_clk   (i_a_clk     ),
    .i_dat_in    (ack         ),
    .o_dat_out   (ack_b2a     )
);

always@(posedge i_a_clk)begin
    if(!i_a_rst_n)
        req <= 1'b0;
    else 
        req <= !ack_b2a;
end        
        
always@(posedge i_b_clk)begin
    if(!i_b_rst_n)
        ack <= 1'b0;
    else 
        ack <= req_a2b;
end


always@(posedge i_a_clk)begin
    if(!i_a_rst_n)
        a_bus_ff <= {BUS_WIDTH{1'b0}};
    else if(req == ack_b2a)
        a_bus_ff <= i_a_bus;
    else;
end


always@(posedge i_b_clk)begin
    if(!i_b_rst_n)
        o_b_bus <= {BUS_WIDTH{1'b0}};
    else if(ack != req_a2b)
        o_b_bus <= a_bus_ff;
    else  ;
end

endmodule


    