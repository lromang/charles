#! /bin/bash
./getCatalog.sh
./getDependencies.sh
echo '"conjunto_de_datos","fecha_modif","num_catalogos","num_bases"' >> ../../data/inventory_test.csv
for i in $(cat ../../data/dependencies.txt)
do
    contents=$(curl -ls $i | wc -w)
    if [ $contents -ne 1 ]
    then
        $(echo $i >> ../../data/dependencies_working.txt)
        name_dataset=$(curl -ls $i | jq -c '.["title"]')
        date_modified=$(curl -ls $i | jq -c '.["modified"]')
        number_catalog=$(curl -ls $i | jq -c '.["dataset"] | length')
        number_base=$(curl -ls $i | jq -c '.["dataset"][]["distribution"] | length'| awk '{s+=$1} END {print s}')
        value=$(echo "$name_dataset,$date_modified,$number_catalog,$number_base")
        # length=$(echo $value | awk -F ',' '{print NF}')
        empty=$(echo $value | grep -E '(,,|,$|^,)'| wc -l)
        while [ $empty -ne 0 ]
        do
	    name_dataset=$(curl -ls $i | jq -c '.["title"]')
	    date_modified=$(curl -ls $i | jq -c '.["modified"]')
	    number_catalog=$(curl -ls $i | jq -c '.["dataset"] | length')
	    number_base=$(curl -ls $i | jq -c '.["dataset"][]["distribution"] | length'| awk '{s+=$1} END {print s}')
	    value=$(echo "$name_dataset,$date_modified,$number_catalog,$number_base")
	    # length=$(echo $value | awk -F ',' '{print NF}')
	    empty=$(echo $value | grep -E '(,,|,$|^,)' | wc -l)
        done
        echo $value
        echo $value >> ../../data/inventory_test.csv
    fi
done


