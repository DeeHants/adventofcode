# AoC2025 day 2
while(<>){
    @s=split//;
    $hi = $hc=$hc2=0;
    for ($i = 0; $i < $#s-1; $i++){
        ($hi, $hc) = $s[$i] > $hc ? ($i, $s[$i]) : ($hi, $hc);

    }
    print "$hc at $hi, ";
    for ($i = $hi+1; $i < $#s; $i++){
        ($hi, $hc2) = $s[$i] > $hc2 ? ($i, $s[$i]) : ($hi, $hc2);
    }
    print "$hc2 at $hi\n";
    $s += $hc.$hc2;
}
print "$s $s2\n";
