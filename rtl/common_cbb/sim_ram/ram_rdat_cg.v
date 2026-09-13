///============================================================================
/// FILE NAME    :   gated_sp_ram_wrapper.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-7 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  Single Port gated memory wrapper.                                           
///============================================================================
module ram_rdat_cg#(
    parameter WIDTH     = 32,
    parameter FLOP_OUT  = 0
)(
    input  wire                         clk,
    input  wire                         cg_en,
    output wire [WIDTH -1:0]            rdat
    
    //--------intf to memory
    input  reg  [WIDTH -1:0]            mem_rdat
);

reg  [WIDTH -1:0]   rdata_backup;
reg                 cg_en_ff;
wire [WIDTH -1:0]   rdat_tmp; 
wire                cg_en_posedge;


always@(posedge i_clk)
    cg_en_ff <= cg_en;
    
assign cg_en_posedge = cg_en && !cg_en_ff;
//----------------------------------
always@(posedge i_clk)begin
    if(cg_en_posedge)
        rdata_backup <= mem_rdat;
    else ;
end
//----------------------------------
assign rdat_tmp = cg_en_ff ?  mem_rdat : rdata_backup;

generate
    if(FLOP_OUT == 0)begin:NO_LAT
        assign rdat = rdat_tmp;
    end
    else begin:LAT_MORE
        reg [WIDTH -1:0]    rdat_tmp_ff;
        always@(posedge i_clk)
            rdat_tmp_ff <= rdat_tmp;
        assign rdat = rdat_tmp_ff;
    end
endgenerate

endmodule


        
