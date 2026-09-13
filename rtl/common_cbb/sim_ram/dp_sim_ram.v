///==============================================================================
/// FILE NAME    :   sp_sim_ram.v                                              ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// VERSION                                                                    ##
///       Time              Author           Description                       ##
/// 2015-9-27 21:42:42     Ethan Hao           Create                          ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// Purpose : dual port ram simulation model.                                   ##
///==============================================================================

module dp_sim_ram #(
		parameter WIDTH = 32,
		parameter DEPTH = 16,
		parameter ADDR_W = log2(DEPTH)
		)(  
		//port a
		input                       clk,
		input                       a_we,
		input						a_cs,
		input       [ADDR_W -1:0]   a_addr,
		input		[WIDTH -1:0]	a_wdat,
		output reg	[WIDTH -1:0]	a_rdat,
		
		input						b_cs,
		input                       b_we,
		input		[ADDR_W -1:0]   b_addr,
		input       [WIDTH -1:0]    b_wdat,
		output reg  [WIDTH -1:0]    b_rdat
		);

	reg [WIDTH -1:0]    mem[DEPTH -1:0];

	integer i;
	always@(posedge clk)begin
		for(i=0;i<DEPTH;i=i+1)begin:LOOP_EACH_ENTRY
			if(a_we & a_cs & a_addr == i)
				mem[i] <= a_wdat;
			else if(b_we & b_cs & b_addr == i)
				mem[i] <= b_wdat;
			else ;
		end
	end

	always@(posedge clk)begin
		if(a_cs & !a_we)
			a_rdat <= mem[a_addr];
		if(b_cs & !b_we)
			b_rdat <= mem[b_addr];
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
`ifdef ASSERT_ON
		wire conflict = a_cs & a_we & b_cs & b_we & a_addr == b_addr;
	property write_conflict;
		@(posedge clk)not conflict;
	endproperty
	
	assert_write_conflict:assert property(@(posedge clk) write_conflict)
						  else
						  		$display("ERROR: RAM write and read at the same time !!");
`endif
endmodule




