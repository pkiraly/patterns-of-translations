#!/usr/bin/env bash

source secret.sh

NAME=$1
curl -s "http://api.geonames.org/search?name_equals=$NAME&featureClass=P&type=json&username=$USER" \
   | jq '.geonames[0,1,2,3] | [ .geonameId, .toponymName, .countryName, .lat, .lng, .adminCode1 ]' \
   | paste - - - - - - - - \
   | sed -r 's,(\[|]|\t|\,),,g' \
   | sed 's/  /,/g' \
   | sed "s,^,\"$NAME\","


#  | jq '.geonames[0] | .geonameId, .toponymName, .countryName, .lat, .lng' \
#  | paste -s -d ','
