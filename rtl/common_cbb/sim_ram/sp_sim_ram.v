///==============================================================================
/// FILE NAME    :   sp_sim_ram.v                                              ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// VERSION                                                                    ##
///       Time              Author           Description                       ##
/// 2015-9-27 23:53:42     Ethan Hao           Create                          ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// Purpose : single port ram simulation model.                                ##
///==============================================================================

module sp_sim_ram #(
        parameter WIDTH = 32,
        parameter DEPTH = 16,
        parameter ADDR_W = log2(DEPTH)
)(  
        input                       clk,
        input                       we,
        input                       cs,
        input       [ADDR_W -1:0]   addr,
        input       [WIDTH -1:0]    wdat,
        output reg  [WIDTH -1:0]    rdat
);

reg [WIDTH -1:0]    mem[DEPTH -1:0];

always@(posedge clk)begin
    if(we & cs)
        mem[addr] <= wdat;
end

always@(posedge clk)begin
    if(cs & !we)
        rdat <= mem[addr];
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




