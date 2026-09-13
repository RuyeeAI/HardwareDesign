///==============================================================================
/// FILE NAME    :   pll.v                                                     ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// VERSION                                                                    ##
///       Time              Author           Description                       ##
/// 2013-9-2 23:53:42     Ethan Hao           Create                           ##
///+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-##
/// Purpose : PLL simulation model.                                            ##
///==============================================================================
//`timescale 1ns/100ps 
module tb_pll;
	reg         ref_clk;
	wire        out_clk;

	initial begin 
		ref_clk = 1'd0;
		forever begin
			ref_clk = #4 ~ref_clk;
		end
	end
	initial begin
		$monitor("%0dns :\$monitor: a=%b"  , $stime, out_clk);
	end
	pll pll_u0(
			.ref_clk        (ref_clk),
			.out_clk        (out_clk)
		);

	//$monitor("out_clk change to %b @%d",out_clk,time);

endmodule
