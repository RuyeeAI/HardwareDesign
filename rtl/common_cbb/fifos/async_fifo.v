///============================================================================
/// FILE NAME    :   sync_fifo.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-3 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  fifo controller.                                           
///============================================================================
module async_fifo#(
    parameter   FIFO_DEPTH      = 16,  //can only be even number
    parameter   SYNC2           = 0,
    parameter   ADDR_W          = log2(FIFO_DEPTH),
    parameter   FIFO_FUL_D      = 1<<ADDR_W,
    parameter   FIFO_PTR_BIN_S  = FIFO_FUL_D - FIFO_DEPTH
    
)(
    input  wire                             wr_clk,
    input  wire                             wr_rst_n,
    
    input  wire                             rd_clk,
    input  wire                             rd_rst_n,

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
    output reg  [ADDR_W:0]                  wcnt,
    output reg  [ADDR_W:0]                  rcnt,

    output wire                             mem_re,
    output wire [ADDR_W-1:0]                mem_raddr,
    output wire                             mem_we,
    output wire [ADDR_W-1:0]                mem_waddr
);

reg [ADDR_W:0]                       wptr;
reg [ADDR_W:0]                       wptr_next;
reg [ADDR_W:0]                       rptr;
reg [ADDR_W:0]                       rptr_next;
wire[ADDR_W:0]                       wptr_offset;
wire[ADDR_W:0]                       rptr_offset;
reg [ADDR_W:0]                       wptr_gray;
wire[ADDR_W:0]                       wptr_gray_next;
reg [ADDR_W:0]                       rptr_gray;
wire[ADDR_W:0]                       rptr_gray_next;
wire[ADDR_W:0]                       wptr_gray_rd;
wire[ADDR_W:0]                       rptr_gray_wr;
reg [ADDR_W:0]                       wptr_bin_rd;
reg [ADDR_W:0]                       rptr_bin_wr;
wire[ADDR_W:0]                       wptr_bin_rd_offset;
wire[ADDR_W:0]                       rptr_bin_wr_offset;
integer i;
//reg [ADDR_W:0]                       wcnt;
//reg [ADDR_W:0]                       rcnt;

//---THE write pointer--
always@(*)begin
    if(fifo_we & !full)begin
        if(wptr[ADDR_W-1:0] == (FIFO_DEPTH -1))
            wptr_next = {~wptr[ADDR_W],{ADDR_W{1'b0}}};
        else    
            wptr_next = wptr + 1'd1;
    end
    else 
        wptr_next = wptr;
end

always@(posedge wr_clk)begin
    if(~wr_rst_n)
        wptr <= {(ADDR_W+1){1'b0}};
    else
        wptr <= wptr_next;
end

//-------Read pointer---------
always@(*)begin
    if(fifo_re & !empty)begin
        if(rptr[ADDR_W-1:0] == (FIFO_DEPTH -1))
            rptr_next = {~rptr[ADDR_W],{ADDR_W{1'b0}}};
        else    
            rptr_next = rptr + 1'd1;
    end
    else 
        rptr_next = rptr;
end

always@(posedge rd_clk)begin
    if(~rd_rst_n)
        rptr <= {(ADDR_W+1){1'b0}};
    else
        rptr <= rptr_next;
end


assign mem_we = fifo_we & !full;
assign mem_waddr = wptr[ADDR_W-1:0];
assign mem_re = fifo_re & !empty;
assign mem_raddr = rptr[ADDR_W-1:0];

assign wptr_offset = !wptr[ADDR_W]?(wptr+FIFO_PTR_BIN_S):wptr;
assign rptr_offset = !rptr[ADDR_W]?(rptr+FIFO_PTR_BIN_S):rptr;

assign wptr_gray_next = wptr_offset ^ {1'b0,wptr_offset[(ADDR_W+1) -1:1]};
assign rptr_gray_next = rptr_offset ^ {1'b0,rptr_offset[(ADDR_W+1) -1:1]};


always@(posedge wr_clk)begin
    if(!wr_rst_n)
        wptr_gray <= {(ADDR_W+1){1'b0}};
    else
        wptr_gray <= wptr_gray_next;
end

always@(posedge rd_clk)begin
    if(!rd_rst_n)
        rptr_gray <= {(ADDR_W+1){1'b0}};
    else
        rptr_gray <= rptr_gray_next;
end

synchronizer #(
    .BUS_WIDTH   (ADDR_W+1    ),
    .SYNC2       (SYNC2       )
)U_WPTR2RD_SYNC(
    .i_dst_clk   (rd_clk      ),
    .i_dat_in    (wptr_gray   ),
    .o_dat_out   (wptr_gray_rd)
);

synchronizer #(
    .BUS_WIDTH   (ADDR_W+1    ),
    .SYNC2       (SYNC2       )
)U_RPTR2WR_SYNC(
    .i_dst_clk   (wr_clk      ),
    .i_dat_in    (rptr_gray   ),
    .o_dat_out   (rptr_gray_wr)
);

always@(*)begin
    for(i=0;i<(ADDR_W+1);i=i+1)begin:GEN_BIN
        wptr_bin_rd[i] = ^(wptr_gray_rd>>i);
        rptr_bin_wr[i] = ^(rptr_gray_wr>>i);
    end
end

assign wptr_bin_rd_offset = !wptr_bin_rd[ADDR_W] ? (wptr_bin_rd - FIFO_PTR_BIN_S) : wptr_bin_rd;
assign rptr_bin_wr_offset = !rptr_bin_wr[ADDR_W] ? (rptr_bin_wr - FIFO_PTR_BIN_S) : rptr_bin_wr;

always@(*)begin
    if(wptr_bin_rd_offset[ADDR_W]==rptr_next[ADDR_W])
        rcnt = wptr_bin_rd_offset[ADDR_W-1:0] - rptr_next[ADDR_W-1:0];
    else
        rcnt = wptr_bin_rd_offset[ADDR_W-1:0] - rptr_next[ADDR_W-1:0] + FIFO_DEPTH;
end

always@(*)begin
    if(wptr_next[ADDR_W]==rptr_bin_wr_offset[ADDR_W])
        wcnt = wptr_next[ADDR_W-1:0] - rptr_bin_wr_offset[ADDR_W-1:0];
    else
        wcnt = wptr_next[ADDR_W-1:0] - rptr_bin_wr_offset[ADDR_W-1:0] + FIFO_DEPTH;
end



always@(posedge wr_clk)begin
    if(!wr_rst_n)
        full <= 1'b0;
    else if(wcnt > FIFO_DEPTH -1)
        full <= 1'b1;
    else 
        full <= 1'b0;
end
       
always@(posedge rd_clk)begin
    if(!rd_rst_n)
        empty <= 1'b1;
    else if(rcnt == {ADDR_W{1'b0}})
        empty <= 1'b1;
    else 
        empty <= 1'b0;
end


always@(posedge wr_clk)begin
    if(~wr_rst_n)
        aful <= 1'd0;
    else if(wcnt >= fifo_aful_gap)
        aful <= 1'd1;
    else ;
end
       
always@(posedge wr_clk)begin
    if(~wr_rst_n)
        aempt <= 1'd0;
    else if(rcnt <= fifo_aempt_gap)
        aempt <= 1'd1;
    else 
        aempt <= 1'b0;
end

always@(posedge wr_clk)begin
    if(~wr_rst_n)
        fifo_overflow <= 1'b0;
    else 
        fifo_overflow <= fifo_we && full;
end

always@(posedge rd_clk)begin
    if(~rd_rst_n)
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