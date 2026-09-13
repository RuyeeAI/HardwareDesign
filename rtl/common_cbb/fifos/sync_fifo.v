///============================================================================
/// FILE NAME    :   sync_fifo.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-3 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  fifo controller.                                           
///============================================================================
module sync_fifo#(
    parameter   FIFO_DEPTH = 16,
    parameter   ADDR_W     = log2(FIFO_DEPTH)
)(
    input  wire                             clk,
    input  wire                             rst_n,

    input  wire                             fifo_we,
    input  wire                             fifo_re,

    input  wire [ADDR_W:0]                  fifo_aful_gap,
    input  wire [ADDR_W:0]                  fifo_aempt_gap,

    output reg                              fifo_underflow,
    output reg                              fifo_overflow,

    output reg                              empty,
    output reg                              full,
    output reg                              aful,
    output reg                              aempt,
    output wire [ADDR_W:0]                  fifo_cnt,

    output wire                             mem_re,
    output wire [ADDR_W-1:0]                mem_raddr,
    output wire                             mem_we,
    output wire [ADDR_W-1:0]                mem_waddr
);

reg [ADDR_W -1:0]                       wptr;
reg [ADDR_W -1:0]                       rptr;
reg [ADDR_W:0]                          cnt;
wire[ADDR_W -1:0]                       xxx;
assign xxx= rptr-wptr;
//---THE write pointer--
always@(posedge clk)begin
    if(~rst_n)
        wptr <= {ADDR_W{1'd0}};
    else if(fifo_we & !full)
        wptr <= wptr == FIFO_DEPTH ? {ADDR_W{1'd0}} : wptr + 1'd1;
    else ;
end

always@(posedge clk)begin
    if(~rst_n)
        rptr <= {ADDR_W{1'd0}};
    else if(fifo_re & !empty)
        rptr <= rptr == FIFO_DEPTH ? {ADDR_W{1'd0}} :rptr + 1'd1;
    else ;
end

assign mem_we = fifo_we & !full;
assign mem_re = fifo_re & !empty;
assign mem_raddr = rptr;
assign mem_waddr = wptr;
always@(posedge clk)begin
    if(~rst_n)begin
        cnt <= {(ADDR_W+1){1'd0}};
    end
    else begin
        case({mem_re,mem_we})
            2'b01:cnt<=cnt+1'd1;
            2'b10:cnt<=cnt-1'd1;
            default:cnt<=cnt;
        endcase
    end
end

always@(posedge clk)begin
    if(~rst_n)begin
        full <= 1'd0;
    end
    else begin
        if(cnt == (FIFO_DEPTH-1) & fifo_we & !fifo_re)
            full <= 1'd1;
        else if(cnt == FIFO_DEPTH & !fifo_we & fifo_re)
            full <= 1'd0;
        else ;
    end
end

always@(posedge clk)begin
    if(~rst_n)begin
        empty <= 1'd1;
    end
    else begin
        if((cnt == 1) & (!fifo_we) & fifo_re)
            empty <= 1'd1;
        else if((cnt == 0) & fifo_we & !fifo_re)
            empty <= 1'd0;
        else ;
    end 
end

always@(posedge clk)begin
    if(~rst_n)
        aful <= 1'd0;
    else if(cnt == (fifo_aful_gap -1) && fifo_we && !fifo_re)
        aful <= 1'd1;
    else if(cnt == fifo_aful_gap && fifo_re && !fifo_we)
        aful <= 1'b0;
    else ;
end


always@(posedge clk)begin
    if(~rst_n)
        aempt <= 1'd1;
    else if((cnt == fifo_aempt_gap +1) && fifo_re && !fifo_we)
        aempt <= 1'd1;
    else if((cnt == fifo_aempt_gap) && fifo_we && !fifo_re)
        aempt <= 1'b0;
    else ;
end

always@(posedge clk)begin
    if(~rst_n)
        fifo_overflow <= 1'b0;
    else 
        fifo_overflow <= fifo_we && full;
end

always@(posedge clk)begin
    if(~rst_n)
        fifo_underflow <= 1'b0;
    else
        fifo_underflow <= fifo_re && empty;
end

assign fifo_cnt = cnt;

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

