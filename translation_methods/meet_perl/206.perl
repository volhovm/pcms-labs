while (<>) {
  s/(\w)\g{-1}/$1/g;
  print; 
}
