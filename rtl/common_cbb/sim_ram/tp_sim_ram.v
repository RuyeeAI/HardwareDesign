///==============================================================================
/// FILE NAME    :   sp_sim_ram.v                                              ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// VERSION                                                                    ##
///       Time              Author           Description                       ##
/// 2015-9-27 21:42:42     Ethan Hao           Create                          ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// Purpose : two port ram simulation model.                                   ##
///==============================================================================

module tp_sim_ram #(
        parameter WIDTH = 32,
        parameter DEPTH = 16,
        parameter RD_LATENCY = 3,
        parameter ADDR_W = log2(DEPTH)
        )(  
        input                       wclk,
        input                       rclk,
        input                       we,
        input                       re,
        input       [ADDR_W -1:0]   waddr,
        input       [ADDR_W -1:0]   raddr,
        input       [WIDTH -1:0]    wdat,
        output      [WIDTH -1:0]    rdat
        );

    reg [WIDTH -1:0]    mem[DEPTH -1:0];
    reg [WIDTH -1:0]    rdat_ff[RD_LATENCY -1:0];

    always@(posedge wclk)begin
        if(we)
            mem[waddr] <= wdat;
    end

    //always@(posedge rclk)begin
    //    if(re)
    //        rdat <= mem[raddr];
    //end
    
    generate 
    genvar i;
    for(i=0;i<RD_LATENCY;i=i+1)begin
        if(i==0)begin
            always@(posedge rclk)
                if(re)
                    rdat_ff[i] <= mem[raddr];
                else
                    rdat_ff[i] <= {WIDTH{1'bx}};
        end
        else begin
            always@(posedge rclk)
                rdat_ff[i] <= rdat_ff[i-1];
        end
    end
    endgenerate
    assign rdat = rdat_ff[RD_LATENCY-1];
    //----------------------------
    function integer log2;
        input integer i;
        begin
            log2=1;
            while(2**log2 <i)begin
                log2=log2+1;
            end
        end
    endfunction
    //----------------------------      

endmodule




