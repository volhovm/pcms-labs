#!/bin/sh

echo "100 100 50 1000" > problem4.in
for i in {50..99}; do echo -ne "$i " >> problem4.in; done
echo "" >> problem4.in
for i in {0..100};
do
    rand1=$(( $RANDOM % 100 + 1 ))
    rand2=$(( $RANDOM % 100 + 1 ))
    rand3=$(( ($RANDOM % 23) + 97 ))
    echo -ne "$rand1 $rand2 " >> problem4.in
    printf "\x$(printf %x $rand3)" >> problem4.in
    echo "" >> problem4.in
done
