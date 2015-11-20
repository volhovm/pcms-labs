#!/bin/sh

echo "10 100000" > mutation.in
for i in {1..100000};
do
    echo $i
    echo "1011010010" >> mutation.in
    echo "0110110100" >> mutation.in
done
