#!/bin/bash

# should be >6
number=20
eps_number=5

function rand_big_char()
{
    local randnonterm=$(($RANDOM % 26 + 65))
    local res=$(perl -e 'printf "%c\n", '$randnonterm)
    echo "$res"
}

function generate_terminals()
{
    local length=$(( $RANDOM % 10 ))
    local retstr=""
    for j in $(seq 0 $length);
    do
        retstr=$retstr$(rand_big_char)
    done
    echo $retstr
}

clang++ -g -std=c++11 -o a.out B_epsilon.cpp
echo "compiled 1"
clang++ -g -std=c++11 -o b.out B_epsilon2.cpp
echo "compiled 2"

while [ 0 ];
do
    echo "$(($number + $eps_number)) A" > epsilon.in
    for i in $(seq 1 $number);
    do
        randchar=$(rand_big_char)
        echo "$randchar -> $(generate_terminals)" >> epsilon.in
    done

    for i in $(seq 1 $eps_number);do echo "$(rand_big_char) ->" >> epsilon.in; done

    ./a.out
    cp epsilon.out epsilon1.out
    ./b.out
    cp epsilon.out epsilon2.out

    diff epsilon1.out epsilon2.out
    if [ $? -ne 0 ]; then exit; fi
done
