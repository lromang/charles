#! /bin/bash

for i in $(ls ../../data); do
	wc -c  "../../data/$i" | awk '{print $1}'>> weight.txt
done
