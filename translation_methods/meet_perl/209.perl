while (<>) {
  s/\([^\)]*\)/\(\)/g;
  print; 
}
