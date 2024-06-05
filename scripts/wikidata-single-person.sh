#!/usr/bin/env bash

ID=$1

curl -s -X POST -H 'Accept: text/csv' https://query.wikidata.org/sparql -d "query= 
SELECT ?sexLabel ?dateOfBirth ?placeOfBirthLabel ?placeOfBirthGeonamesId ?placeOfBirthCoord ?dateOfDeath ?placeOfDeathLabel ?placeOfDeathGeonamesId ?placeOfDeathCoord ?viaf ?gndID WHERE { 
  wd:$ID
    wdt:P21 ?sex ;  
    wdt:P569 ?dateOfBirth ; 
  OPTIONAL { 
    wd:$ID wdt:P570 ?dateOfDeath . 
  }
  OPTIONAL { 
    wd:$ID wdt:P19 ?placeOfBirth . 
    ?placeOfBirth wdt:P1566 ?placeOfBirthGeonamesId . 
    ?placeOfBirth wdt:P625 ?placeOfBirthCoord . 
  }
  OPTIONAL { 
    wd:$ID wdt:P20 ?placeOfDeath . 
    ?placeOfDeath wdt:P1566 ?placeOfDeathGeonamesId . 
    ?placeOfDeath wdt:P625 ?placeOfDeathCoord . 
  }
  OPTIONAL { wd:$ID wdt:P214 ?viaf. } 
  OPTIONAL { wd:$ID wdt:P227 ?gndID. } 
  SERVICE wikibase:label { bd:serviceParam wikibase:language 'hu' . } 
}" \
  | grep -v Label \
  | sed 's/T00:00:00Z//g' \
  | sed -r 's/Point\((-?[0-9]+\.[0-9]+) (-?[0-9]+\.[0-9]+)\)/\1,\2/g' \
  | sed -r "s,^,https://www.wikidata.org/wiki/$ID\,,"
