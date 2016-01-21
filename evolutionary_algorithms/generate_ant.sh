#!/bin/sh

filename="artificial.py"
echo -n "" > $filename
for i in $(seq 1 10); do
        echo -n "solution$i=\"\"\"" >> $filename
        cat artificialtests/test$i.out >> $filename
        echo "\"\"\"" >> $filename
done
echo "" >> $filename
cat "$filename".common >> $filename
