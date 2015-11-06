#! /bin/bash

rm ../../data/dependencies.txt ../../data/catalogs.json ../../data/inventory_test.csv ../../data/MAT_test.csv
chmod u+x *.sh
./getDatasets.sh
./getMAT.sh
