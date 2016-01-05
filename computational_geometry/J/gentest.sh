#!/bin/sh

max=1000000
echo 1000001 > tower.in
for i in {0..1000000}; do
    echo $i
    echo -n "$(( $RANDOM % $max )) " >> tower.in
    echo $(( $RANDOM % $max)) >> tower.in
done
