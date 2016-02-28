while (<>) {
  print if /(x|y|z)\w{5,17}(x|y|z)/;
}
