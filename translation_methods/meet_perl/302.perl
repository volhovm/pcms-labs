$global = "";

foreach $line ( <STDIN> ) {
    $line =~ s/<.*?>//g;
    $line =~ s/^\s+//g;
    $line =~ s/\s+$//g;
    $line =~ s/(\s){2,}/ /g;
    $global = $global . $line . "\n"; 
}

$global =~ s/(\n){2,}/\n\n/g;
$global =~ s/^(\n)+//g;
$global =~ s/(\n)+$//g;

print $global;
