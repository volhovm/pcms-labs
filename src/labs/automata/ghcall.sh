#!/bin/sh
for i in $(ls *.hs);
do
	ghc $i -o $i.run
	rm *.hi
	rm *.o
done
