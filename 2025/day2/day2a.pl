# AoC2025 day 2 part 1
$/=',';while(<>){/(\d+)-(\d+)/;map{$l=int(length()/2);/^(\d{$l})(\d{$l})$/;$s+=$1 eq $2?$_:0}($1..$2)}print"$s\n";
