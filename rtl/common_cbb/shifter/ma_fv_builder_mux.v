///============================================================================
/// FILE NAME    :   ma_fv_builder_mux.v                                              
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// VERSION                                                                    
///       Time              Author           Description                       
/// 2015-10-3 23:53:42     Ethan Hao           Create                          
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
/// Purpose :  this module is used to merge the src data to dst data according 
///            to the profile, which compose of dest ID and byte enable.                                      
///============================================================================
module ma_fv_builder_mux #(
    parameter SRC_DATA_W    = 1024,
    parameter SEG_W         = 64,
    parameter DST_DATA_W    = 2560,
    //parameter below should not changed when instance
    parameter SRC_ID_W      = log2(DST_DATA_W/SEG_W),
    parameter BYTE_EN_W     = SEG_W/8,
    parameter PRF_SEG_W     = SRC_ID_W + BYTE_EN_W,
    parameter SRC_SEG_NUM   = SRC_DATA_W/SEG_W,
    parameter PRF_W         = PRF_SEG_W*SRC_SEG_NUM,
    parameter DST_SEG_NUM   = DST_DATA_W/SEG_W,
    parameter DST_BYTE_NUM  = DST_DATA_W/8,
    parameter END_OF_LIST   = 1
)(
    input  wire [SRC_DATA_W -1:0]           i_src_dat,
    input  wire [PRF_W -1:0]                i_prf_dat,
    input  wire [DST_DATA_W -1:0]           i_old_dat,
    output wire [DST_DATA_W -1:0]           o_updated_dat
);
 
wire    [SRC_ID_W -1:0]                     seg_id[SRC_SEG_NUM-1:0];
wire    [BYTE_EN_W -1:0]                    byte_en[SRC_SEG_NUM-1:0];
wire    [DST_SEG_NUM -1:0]                  seg_dest_en[SRC_SEG_NUM -1:0];
wire    [SEG_W -1:0]                        src_seg[SRC_SEG_NUM -1:0];
reg     [BYTE_EN_W -1:0]                    update_flag[DST_SEG_NUM-1:0];
wire    [SEG_W -1:0]                        old_seg[DST_SEG_NUM -1:0];
reg     [DST_BYTE_NUM -1:0]                 dest_byte_en[SRC_SEG_NUM -1:0];
reg     [SEG_W -1:0]                        fv_seg[DST_SEG_NUM -1:0];
integer i;
integer j;
generate
genvar pi;
for(pi=0;pi<SRC_SEG_NUM;pi=pi+1)begin:EXTRACT_SRC_DATA
    assign {seg_id[pi],byte_en[pi]} = i_prf_dat[PRF_W -1 -PRF_SEG_W*pi -:PRF_SEG_W];
    assign seg_dest_en[pi] = {1'b1,{(DST_SEG_NUM -1){1'b0}}}>>seg_id[pi];
    assign src_seg[pi] = i_src_dat[SRC_DATA_W -1 -SEG_W*pi -:SEG_W];
end

genvar fi;
genvar bi;
for(fi=0;fi<DST_SEG_NUM;fi=fi+1)begin:MERGE_DST_DATA
    assign old_seg[fi] = i_old_dat[SEG_W*fi+:SEG_W];
    always@(*)begin
        update_flag[fi] = {BYTE_EN_W{1'b0}};
        for(i=0;i<SRC_SEG_NUM;i=i+1)begin:UPDATE_FLAG
            dest_byte_en[i][fi*BYTE_EN_W+:BYTE_EN_W] = {BYTE_EN_W{seg_dest_en[i][fi]}} & byte_en[i];
            update_flag[fi] = update_flag[fi] | dest_byte_en[i][fi*BYTE_EN_W+:BYTE_EN_W];
        end
    end
        
    for(bi=0;bi<BYTE_EN_W;bi=bi+1)begin:LOOP_BYTE
        always@(*)begin
            fv_seg[fi][bi*8 +:8] = {8{!update_flag[fi][bi]}} & old_seg[fi][bi*8+:8];
            for(j=0;j<SRC_SEG_NUM;j=j+1)begin:MERGE_DAT
                fv_seg[fi][bi*8 +:8] = fv_seg[fi][bi*8+:8] | ({8{dest_byte_en[j][fi*BYTE_EN_W +bi]}} & src_seg[j][bi*8+:8]);
            end
        end
    end
    
    assign o_updated_dat[SEG_W*fi+:SEG_W] = fv_seg[fi];
end
endgenerate

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

