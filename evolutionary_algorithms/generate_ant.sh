#!/bin/sh

filename="artificial.py"
echo -n "" > $filename
for i in $(seq 1 8); do
        echo "solution$i=\"\"\"" >> $filename
        cat artificialtests/test$i.out >> $filename
        echo "\"\"\"" >> $filename
done
