#! /bin/bash
echo "institution,base_name,url,type,size" >> ../../data/MAT_test.csv
for i in $(cat ../../data/dependencies_working.txt)
do
    empty=1
    while [ $empty -ne 0 ]
    do
	inst=$(echo $i | awk -F '/' '{print $2}')
	title=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["title"]' )
	url=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["downloadURL"]' )
	type=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["mediaType"]' )
	size=$(curl -ls $i | jq -c '.["dataset"][]["distribution"][] | .["byteSize"]' )
	length=$(echo "$title" | wc -l)
	#### Caso especial San Luis
	sanLuis=$(echo "$inst" | grep -E 'uis'| wc -l)
        zap=$(echo "$inst" | grep -E 'zapop'| wc -l)
	if [ $sanLuis -eq 0 -a $zap -eq 0 ]
	then
	    #############################
	    for j in  `seq 1 $length`
	    do
		title_j=$(echo "$title" | sed -n  "${j}p")
		url_j=$(echo "$url"     | sed -n  "${j}p")
		type_j=$(echo "$type"   | sed -n  "${j}p")
		size_j=$(echo "$size"   | sed -n  "${j}p")
		value=$(echo "\"$inst\",$title_j,$url_j,$type_j,$size_j")
		echo $value
		empty=$(echo $value | grep -E '(,,|,$|^,)' | wc -l)
		if [ $empty -eq 0 -a $zap -eq 0 ]
		then
		    echo $value >> ../../data/MAT_test.csv
		fi
	    done
	else
	    empty=0
	fi
    done
done
