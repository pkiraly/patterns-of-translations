#!/usr/bin/env bash

FILE="../data/authors-wikiIDs.csv"
echo "wiki,sex,dateOfBirth,placeOfBirth,placeOfBirthX,placeOfBirthY,dateOfDeath,placeOfDeath,placeOfDeathX,placeOfDeathY,viaf,gndID"
while IFS= read -r LINE
do
  if [[ "$LINE" != "wiki" ]]; then
  	ID=$(echo $LINE | sed -r 's,https://www.wikidata.org/wiki/,,')
  	./wikidata-single-person.sh $ID
	fi
done < "$FILE"
