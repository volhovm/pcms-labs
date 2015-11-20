#!/bin/bash

range=3
rangey=5
intersection=3
echo $(( 4 * $range + 2 )) > input.txt

function onOneLine () {
    local ax=$(( $3 - $1 ))
    local ay=$(( $4 - $2 ))
    local bx=$(( $5 - $3 ))
    local by=$(( $6 - $4 ))

    local turnres=$(( $ax * $by - $ay * $bx ))

    if [ $turnres -eq 0 ]; then
        return 0
    else
        return 1
    fi
}

function getRandY () {
    if [ $(( $1 % 2 )) -eq 0 ]; then
        randomY=$(( -($RANDOM % $rangey) + $intersection))
    else
        randomY=$(( -($RANDOM % $rangey) ))
    fi
}

function getRandYpos () {
    if [ $(( $1 % 2 )) -eq 0 ]; then
        randomY=$(( ($RANDOM % $rangey) + $intersection))
    else
        randomY=$(( ($RANDOM % $rangey) ))
    fi
}

prevx=-228228
prevy=-228228
prevprevx=-228228
prevprevy=-228228

# lower bound
for i in `seq $(( -$range )) $range`; do
    if [ $prevx -ne -228228 -a $prevprevx -ne -228228 ]; then
        getRandY $i;
        while ( onOneLine $prevprevx $prevprevy $prevx $prevy $i $randomY ); do
            getRandY $i;
        done
        echo "$i $randomY" >> input.txt
    else
        getRandY $i;
        echo "$i $randomY" >> input.txt
    fi

    prevprevx=$prevx
    prevprevy=$prevy
    prevx=$i
    prevy=$randomY
done

# upper bound
for i in `seq $(( -$range )) $range`; do
    i=$(( -$i ))

    getRandYpos $i;
    while ( onOneLine $prevprevx $prevprevy $prevx $prevy $i $randomY ); do
        getRandYpos $i;
    done
    echo "$i $randomY" >> input.txt

    prevprevx=$prevx
    prevprevy=$prevy
    prevx=$i
    prevy=$randomY
done
