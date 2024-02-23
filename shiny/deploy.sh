#!/usr/bin/env bash

service shiny-server stop
cp -r network /srv/shiny-server/
service shiny-server start
