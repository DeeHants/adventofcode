# AoC2025 day 3
while(<>){@s=split//;($l,$r,$hi)=(12,'',-1);for($d=0;$d<$l;$d++){$hc=0;for($i=$hi+1;$i<$#s-(($l-$d)-1);$i++){($hi,$hc)=$s[$i]>$hc?($i,$s[$i]):($hi,$hc);}$r.=$hc;}$s+=$r;}
print "$s\n";
