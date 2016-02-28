for i in $(seq 0 1000); do
	b=$(perl -e 'printf qq|%b\n|, int( shift )' $i)
	res=$(echo $b | perl 111.perl)
	if [ $(($i % 3)) == 0 ]; then
	        test $res != $b && (echo "true negative:  $b" && exit)
 	else
	        test $res && (echo "false positive: $b" && exit)
	fi
done
