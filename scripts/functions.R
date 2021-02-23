library(tidyverse)
library(ggplot2)
library("stringdist")

clean_year <- function(year) {
  year <- gsub(
    '^(o\\. ?[jJ]\\.?|s\\. ?a\\.?|b\\. ?r\\.?|Jahr unbek\\.|é\\. n\\.|s\\. ?d\\.|b\\. ?g\\.|Datum fehlt|Datum unbek\\.|Ohne Jahresang\\.|w\\. y\\.|without year|bez\\. god\\.|sans date|[fF]\\. ?a\\.)$',
    NA,
    year
  )
  year <- gsub('^(\\d\\d\\d\\d)([−/]\\d\\d\\d\\d)?$', '\\1', year)
  year
}

clean_city <- function(city) {
  print(dim(city))

  city <- gsub(', [aA]nno', '', city) 
  city <- gsub('[\\.,]$', '', city)
  city <- gsub('(\\(|\\)|\\[|\\])', '', city)
  city <- gsub('[\\.,]$', '', city)
  city <- gsub('^s. l$', 's. l.', city)

  return(city)
}  