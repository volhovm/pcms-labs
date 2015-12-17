#for i in {0..30}; do
#    bytevar=$(echo "obase=2;$i" | bc)
#    factD=$(python -c "import math; print math.factorial($i)")
#    factBpre=$(echo "obase=2;$factD" | bc)
#    factB=$(sed -e "s/\\\ //g" <<< $factBpre)
#    factlength=${#factB}
#    for j in $(seq 0 $(( $factlength - 2 ))); do
#        k=$(($factlength - 1 - $j))
#        factchar=${factB:$k:1}
#        #echo "DEBUG: k: $k factB: $factB factchar: $factchar"
#        echo "writefact_$bytevar t$j -> writefact_$bytevar $factchar <"
#    done
#    factchar=${factB:0:1}
#    echo "writefact_$bytevar t"$(( $factlength - 1 ))" -> cleanup $factchar <"
#done

for i in {0..107}; do
    echo "cleanup t$i -> cleanup _ <"
done
