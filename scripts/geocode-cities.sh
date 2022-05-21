#!/usr/bin/env bash

FILE=$1

if [[ "$FILE" == "" ]]; then
  FILE="../data/city-by-works.csv"
fi

echo "city,geoid,name,country,lat,long"
while IFS= read -r LINE
do
  if [[ "$LINE" != "NA,NA" && "$LINE" != "city,works" ]]; then
  	CITY=$(echo $LINE | sed -r 's/,[0-9\.]+//g' | sed 's/"//g')
  	if [[ "$CITY" != "s. l" && "$CITY" != "s. l." ]]; then
  		ENCODED=$(echo $CITY | sed 's: :%20:g')
  		./geoname.sh $ENCODED
		  #GEO=$(source ./geoname.sh $ENCODED)
		  #echo "\"$CITY\",$GEO"
	  fi
	fi
done < "$FILE"
