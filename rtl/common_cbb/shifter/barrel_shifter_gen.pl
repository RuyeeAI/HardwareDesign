#!/usr/bin/perl -w
# This scripts is used to gen ultra left shifter.
# The parameter should be the width of the shifter.

$src_wid = pop @ARGV;
$module_name = "bs$src_wid";
$shift_bit_wid = int(log($src_wid)/log(2));
open FO,">$module_name.v" or die "Can not open $module_name.v $!";
print FO   "module $module_name(\n";
print FO   "    input   [$src_wid -1:0]                 i_src_dat,\n";
print FO   "    input   [$shift_bit_wid -1:0]           i_shift_bit,\n"; 
print FO   "    output  [$src_wid -1:0]                 o_dst_dat\n);\n\n";
for($i=0;$i<$shift_bit_wid+1;$i++){
    print FO    "wire   [$src_wid -1:0]                 lvl$i\_dat;\n";
}

print FO "assign lvl0_dat = i_src_dat;\n";

for($i=1;$i<$shift_bit_wid+1;$i++){
    $dat_index=$src_wid - 2**($i-1);
    $zw = 2**($i-1);
    print $zw,"\n";
    $ri=$i-1;
    print FO    "assign lvl$i\_dat = i_shift_bit[$ri] ? {lvl$ri\_dat[$dat_index -1:0],$zw\'b0} : lvl$ri\_dat;\n";
}
$i--;   
print FO "assign o_dst_dat = lvl$i\_dat;\n";
print FO "endmodule\n";
close FO;