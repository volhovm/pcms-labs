n=100
m=1000
k=50
echo "$n $m $k" > problem2.in
for i in {0..49};
do
	echo -ne "$(( ($RANDOM % (n - 1)  + 1) )) " >> problem2.in
done
echo "" >> problem2.in
for i in {0..999};
do
	echo -ne "$(( ($RANDOM % (n - 1) + 1 ) )) $(( ($RANDOM % (n - 1) + 1) )) " >> problem2.in
	printf "\x$(printf %x $(( ($RANDOM % 23) + 97 )))" >> problem2.in
	echo "" >> problem2.in
done
