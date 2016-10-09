#!/bin/bash

set -e

make new > /dev/null

for f in examples/*
do
	./summa -a $f > test_all.log
done

echo "All passed ast."

for f in examples/*
do
	./summa -s $f > test_all.log
done

echo "All passed semantic check."