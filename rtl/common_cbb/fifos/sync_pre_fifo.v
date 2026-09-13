///============================================================================
/// FILE NAME    :   sync_fifo.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-3 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  sync prefifo controller.                                           
///============================================================================
module sync_pre_fifo#(
    parameter   FIFO_DEPTH      = 16,
    parameter   FIFO_WIDTH      = 32,
    parameter   MEM_RD_LATENCY  = 2,
    parameter   CACHE_DEPTH     = MEM_RD_LATENCY + 1,
    parameter   MEM_ADDR_W      = log2(FIFO_DEPTH),
    parameter   FIFO_ADDR_W     = log2(FIFO_DEPTH+CACHE_DEPTH),
    parameter   CACHE_ADDR_W    = log2(CACHE_DEPTH)
)(
    input  wire                             clk,
    input  wire                             rst_n,
    
    input  wire                             fifo_we,
    input  wire [FIFO_WIDTH -1:0]           fifo_wdat,
    input  wire                             fifo_re,
    output wire [FIFO_WIDTH -1:0]           fifo_rdat,

    input  wire [FIFO_ADDR_W:0]             fifo_aful_gap,
    input  wire [FIFO_ADDR_W:0]             fifo_aempt_gap,

    output reg                              fifo_underflow,
    output reg                              fifo_overflow,

    output reg                              empty,
    output reg                              full,
    output reg                              aful,
    output reg                              aempt,
//    output wire [FIFO_ADDR_W:0]             fifo_cnt,

    input  wire [FIFO_WIDTH -1:0]           mem_rdat,
    output wire                             mem_re,
    output wire [MEM_ADDR_W-1:0]            mem_raddr,
    output wire [FIFO_WIDTH -1:0]           mem_wdat,
    output wire                             mem_we,
    output wire [MEM_ADDR_W-1:0]            mem_waddr
);

reg     [MEM_ADDR_W-1:0]                    wptr;
reg     [MEM_ADDR_W-1:0]                    rptr;
reg     [MEM_ADDR_W:0]                      mem_cnt;

reg     [CACHE_ADDR_W -1:0]                 c_wptr;
reg     [CACHE_ADDR_W -1:0]                 c_rptr;
reg     [CACHE_ADDR_W:0]                    cache_cnt;

reg     [FIFO_ADDR_W -1:0]                  fifo_cnt;

wire                                        cache_we;
wire                                        cache_re;
wire    [FIFO_WIDTH -1:0]                   cache_wdat;
reg     [FIFO_WIDTH -1:0]                   cache[CACHE_DEPTH-1:0];
reg     [MEM_RD_LATENCY -1:0]               mem_re_ff;
wire                                        cache_full;
wire                                        cache_empty;
wire                                        mem_full;
wire                                        mem_empty;

//---write pointer---
always@(posedge clk)begin
    if(!rst_n)
        wptr <= {MEM_ADDR_W{1'b0}};
    else if(mem_we)
        wptr <= wptr == (FIFO_DEPTH-1) ? {MEM_ADDR_W{1'b0}} : wptr + 1'b1;
    else ;
end
//---read pointer ----           
 always@(posedge clk)begin
    if(!rst_n)
        rptr <= {MEM_ADDR_W{1'b0}};
    else if(mem_re)
        rptr <= rptr == (FIFO_DEPTH-1) ? {MEM_ADDR_W{1'b0}} : rptr + 1'b1;
    else ;
end       
//---Cache write pointer ----
always@(posedge clk)begin
    if(!rst_n)
        c_wptr <= {CACHE_ADDR_W{1'b0}};
    else if(cache_we)
        c_wptr <= c_wptr == (CACHE_DEPTH-1) ? {CACHE_ADDR_W{1'b0}} : c_wptr + 1'b1;
    else ;
end
//----Cache read pointer ----
always@(posedge clk)begin
    if(!rst_n)
        c_rptr <= {CACHE_ADDR_W{1'b0}};
    else if(cache_re)
        c_rptr <= c_rptr == (CACHE_DEPTH-1) ? {CACHE_ADDR_W{1'b0}} : c_rptr + 1'b1;
    else ;
end

always@(posedge clk)begin
    if(!rst_n)
        mem_cnt <= {MEM_ADDR_W{1'b0}};
    else if(mem_we & !mem_re)
        mem_cnt <= mem_cnt + 1'b1;
    else if(!mem_we & mem_re)
        mem_cnt <= mem_cnt - 1'b1;
    else ;
end

always@(posedge clk)begin
    if(!rst_n)
        fifo_cnt <= {FIFO_ADDR_W{1'b0}};
    else if(fifo_we & !fifo_re)
        fifo_cnt <= fifo_cnt + 1'b1;
    else if(!fifo_we & fifo_re)
        fifo_cnt <= fifo_cnt - 1'b1;
    else ;
end

always@(posedge clk)begin
    if(!rst_n)
        cache_cnt <= {(CACHE_ADDR_W+1){1'b0}};
    else if((mem_re|(fifo_we & !cache_full)) & !cache_re)
        cache_cnt <= cache_cnt + 1'b1;
    else if(!(mem_re|(fifo_we & !cache_full)) & cache_re)
        cache_cnt <= cache_cnt - 1'b1;
    else ;
end

assign mem_empty   = mem_cnt == {MEM_ADDR_W{1'b0}};
assign mem_full    = mem_cnt == FIFO_DEPTH;
assign cache_empty = cache_cnt == {CACHE_ADDR_W{1'b0}};
assign cache_full  = cache_cnt == CACHE_DEPTH;

assign mem_re    = fifo_re & !mem_empty & !cache_empty;
assign mem_raddr = rptr;
assign mem_wdat  = fifo_wdat;
assign mem_we    = fifo_we & cache_full & !full;
assign mem_waddr = wptr;

always@(posedge clk)begin
    if(!rst_n)
        mem_re_ff <= {MEM_RD_LATENCY{1'b0}};
    else 
        mem_re_ff <= {mem_re_ff[MEM_RD_LATENCY -2:0],mem_re};
end

assign cache_we = (fifo_we & !cache_full)| mem_re_ff[MEM_RD_LATENCY -1];
assign cache_re = fifo_re & !cache_empty;
assign cache_wdat = mem_re_ff[MEM_RD_LATENCY -1] ? mem_rdat : fifo_wdat;

always@(posedge clk)begin
    if(cache_we)
        cache[c_wptr] <= cache_wdat;
    else;
end
assign fifo_rdat = cache[c_rptr];


always@(posedge clk)begin
    if(~rst_n)begin
        full <= 1'd0;
    end
    else begin
        if(mem_cnt == (FIFO_DEPTH-1) & fifo_we & !fifo_re)
            full <= 1'd1;
        else if(mem_cnt == FIFO_DEPTH & !fifo_we & fifo_re)
            full <= 1'd0;
        else ;
    end
end

always@(posedge clk)begin
    if(~rst_n)begin
        empty <= 1'd1;
    end
    else begin
        if(fifo_cnt == 0 & fifo_we & !fifo_re)
            empty <= 1'd0;
        else if(fifo_cnt == 1 & !fifo_we & fifo_re)
            empty <= 1'd1;
        else ;
    end
end


always@(posedge clk)begin
    if(~rst_n)
        aful <= 1'd0;
    else if(fifo_cnt == (fifo_aful_gap -1) && fifo_we && !fifo_re)
        aful <= 1'd1;
    else if(fifo_cnt == fifo_aful_gap && fifo_re && !fifo_we)
        aful <= 1'b0;
    else ;
end


always@(posedge clk)begin
    if(~rst_n)
        aempt <= 1'd1;
    else if((fifo_cnt == fifo_aempt_gap +1) && fifo_re && !fifo_we)
        aempt <= 1'd1;
    else if((fifo_cnt == fifo_aempt_gap) && fifo_we && !fifo_re)
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
