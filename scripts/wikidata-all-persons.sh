#!/usr/bin/env bash

set -f

FILE="../data/authors-wikiIDs-v2.csv"
echo "wiki,sex,dateOfBirth,placeOfBirth,placeOfBirthX,placeOfBirthY,dateOfDeath,placeOfDeath,placeOfDeathX,placeOfDeathY,viaf,gndID"
while IFS= read -r LINE
do
 	name=($(echo "$LINE" | awk -F',' '{print $1}'))
 	wiki=($(echo "$LINE" | awk -F',' '{print $2}'))
  if [[ "$wiki" != "wiki" ]]; then
  	ID=$(echo $wiki | sed -r 's,https://www.wikidata.org/wiki/,,')
   	echo '#' $(echo "$LINE" | awk -F',' '{print $1}')
  	./wikidata-single-person.sh $ID
	fi
done < "$FILE"
