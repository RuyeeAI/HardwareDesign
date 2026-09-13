///============================================================================
/// FILE NAME    :   synchronizer.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-2 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  synchronizer .                                           
///============================================================================
module synchronizer#(
    parameter BUS_WIDTH = 1,
    parameter SYNC2     = 0
)(
    input                       i_dst_clk,
    input  [BUS_WIDTH -1:0]     i_dat_in,
    output [BUS_WIDTH -1:0]     o_dat_out
);

`ifdef FOR_SIM
    integer period;
    integer t1;
    reg window;
    
    initial begin
        window = 0;
        period = 1000000;
        @(posedge i_dst_clk);
        t1=$time;
        @(posedge i_dst_clk);
        period = $time -t1;
    end
    
    always@(negedge i_dst_clk)begin
        $display("%t---change windows",$time);
        //#0.01;
        #2.8 window=1;
        $display("%t---change windows to %d",$time,window);
        #0.4 window=0;
        $display("%t---change windows to %d",$time,window);
    end
    
    generate
    if(SYNC2==0)begin:B3_PIPE
        reg [BUS_WIDTH -1:0]        dat_in_ff[2:0];
        reg [BUS_WIDTH -1:0]        dat_in_r_ff[1:0];
        reg [BUS_WIDTH -1:0]        dat_in_r;
        always@(i_dat_in)
            dat_in_r = window ? {BUS_WIDTH{1'bx}} : i_dat_in;
        
        always@(dat_in_ff[0])
            dat_in_r_ff[0] = window ? {BUS_WIDTH{1'bx}} : dat_in_ff[0];
            
        always@(dat_in_ff[1])
            dat_in_r_ff[1] = window ? {BUS_WIDTH{1'bx}} : dat_in_ff[1];
        //assign dat_in_r       = window ? {BUS_WIDTH{1'bx}} : i_dat_in;
        //assign dat_in_r_ff[0] = window ? {BUS_WIDTH{1'bx}} : dat_in_ff[0];
        //assign dat_in_r_ff[1] = window ? {BUS_WIDTH{1'bx}} : dat_in_ff[1];

        always@(posedge i_dst_clk)begin
            dat_in_ff[0] <= dat_in_r;
            dat_in_ff[1] <= dat_in_r_ff[0];
            dat_in_ff[2] <= dat_in_r_ff[1];
        end
        assign o_dat_out = dat_in_ff[2];
    end
    else begin:B2_PIPE
        reg [BUS_WIDTH -1:0]        dat_in_ff[1:0];
        reg [BUS_WIDTH -1:0]        dat_in_r_ff;
        reg [BUS_WIDTH -1:0]        dat_in_r;

        always@(i_dat_in)
            dat_in_r = window ? {BUS_WIDTH{1'bx}} : i_dat_in;
        
        always@(dat_in_ff[0])
            dat_in_r_ff = window ? {BUS_WIDTH{1'bx}} : dat_in_ff[0];
            
        always@(posedge i_dst_clk)begin
            dat_in_ff[0] <= dat_in_r;
            dat_in_ff[1] <= dat_in_r_ff;
        end
        assign o_dat_out = dat_in_ff[1];
    end
    endgenerate
`else
    generate
    if(SYNC2==0)begin:A_3_PIPE
        reg [BUS_WIDTH -1:0]        dat_in_ff[2:0];
        always@(posedge i_dst_clk)begin
            dat_in_ff[0] <= i_dat_in;
            dat_in_ff[1] <= dat_in_ff[0];
            dat_in_ff[2] <= dat_in_ff[1];
        end
        assign o_dat_out = dat_in_ff[2];
    end
    else begin:A_2_PIPE
        reg [BUS_WIDTH -1:0]        dat_in_ff[1:0];
        always@(posedge i_dst_clk)begin
            dat_in_ff[0] <= i_dat_in;
            dat_in_ff[1] <= dat_in_ff[0];
        end
        assign o_dat_out = dat_in_ff[1];
    end
    endgenerate
`endif


endmodule
        