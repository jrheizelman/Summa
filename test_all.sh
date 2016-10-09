#!/bin/bash

set -e

make > /dev/null

for f in examples/*
do
	./summa -a $f
done