#!/usr/bin/env bash

source secret.sh

NAME=$1
curl -s "http://api.geonames.org/search?name=$NAME&featureClass=P&type=json&username=$USER" \
  | jq '.geonames[0] | .geonameId, .toponymName, .countryName, .lat, .lng' \
  | paste -s -d ','
