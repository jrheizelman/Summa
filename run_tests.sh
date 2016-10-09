set -e

# make clean > /dev/null
# make > /dev/null

for f in tests/*
do
	echo "./summa $1 $f"
	./summa $1 $f
done