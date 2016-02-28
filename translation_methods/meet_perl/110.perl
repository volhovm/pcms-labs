while (<>) {
  print if /\b(?P<tandem1>\w+)(?P=tandem1)\b/;
}
