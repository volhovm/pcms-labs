$np = "[^\(\)]*";
while (<>) {
  print if /\($np\b\w+\b$np\)/;
}
