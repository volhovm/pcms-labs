$A = "(00)";
$B = "(11)";
$C = "(01)";
$D = "(10)";
$E = "($A|0$B+0)";
$F = "(0$B*0)";
$G = "($C$B*0)";
$H = "(0$B*$D)";
$I = "($F|$H$E*$G)";
$J = "($H$E*1)";
$K = "(1$E*$G)";
$L = "(1$E*1)";
$M = "(1$I*1)";
$N = "($L|$K$I*$J)";
$P = "(0|$K$I*1)";
$R = "(0|1$I*$J)";
$F = "($M|$R$N*|$R$N*$P)";
while (<>) {
  print if /^$F+$/;
}