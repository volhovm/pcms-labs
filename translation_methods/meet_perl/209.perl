while (<>) {
  s/\([^\)]*\)/\(\)/g;
  print; 
}