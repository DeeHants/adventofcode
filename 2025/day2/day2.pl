# AoC2025 day 2
$/=',';while(<>){/(\d+)-(\d+)/;map{for($l=int(length()/2);$l>0;$l--){$c=int(length()/$l);$p=substr$_,0,$l;if($_ eq $p x $c){$s2+=$_;if($c==2){$s+=$_}last}}}($1..$2)}print "$s $s2\n";
