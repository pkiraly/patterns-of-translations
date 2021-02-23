library(tidyverse)
library(ggplot2)
library("stringdist")
source("scripts/functions.R")

df <- read_csv('data_raw/demeter.csv')
cities <- read_csv('data_raw/normalized-cities.csv')

df %>% 
  mutate(
    is_series = ifelse(
      grepl('^"[^"]+" (\\d\\d\\d\\d([−/]\\d\\d\\d\\d)?\\.|s\\. a\\.)', megjelenes),
      1, 0
    ),
    series = ifelse(
      is_series == 1,
      gsub('^"([^"]+)".*$', '\\1', megjelenes),
      ''
    ),
    city = ifelse(
      is_series == 1,
      '',
      gsub(
        '^(.*?) \\(?(\\d\\d\\d(\\d|\\?)([-−]\\d\\d\\d(\\d|\\?))?|19\\?\\?|o\\. ?[jJ]\\.?|s\\. ?a\\.?|b\\. ?r\\.?|Jahr unbek\\.|é\\. n\\.|s\\. ?d\\.|b\\. ?g\\.|Datum fehlt|Datum unbek\\.|Ohne Jahresang\\.|w\\. ?y\\.|without year|bez\\. god\\.|sans date|[fF]\\. ?a\\.).*',
        '\\1', megjelenes
      )
    ),
    year = ifelse(
      is_series == 1,
      gsub('^"[^"]+" (\\d\\d\\d\\d([−/]\\d\\d\\d\\d)?|s\\. a\\.).*$', '\\1', megjelenes),
      gsub(
        '^(.*?) \\(?(\\d\\d\\d(\\d|\\?)(-\\d\\d\\d(\\d|\\?))?|19\\?\\?|o\\. ?[jJ]\\.?|s\\. ?a\\.?|b\\. ?r\\.?|Jahr unbek\\.|é\\. n\\.|s\\. ?d\\.|b\\. ?g\\.|Datum fehlt|Datum unbek\\.|Ohne Jahresang\\.|w\\. ?y\\.|without year|bez\\. god\\.|sans date|[fF]\\. ?a\\.).*',
        '\\2', megjelenes)
    ),
  ) %>% 
  
  mutate(year = clean_year(year)) %>% 
  mutate(city = clean_city(city)) %>% 
  mutate(city = gsub('Postsdam', 'Potsdam', city)) %>% 
  mutate(city = gsub('Potsdamm', 'Potsdam', city)) %>% 
  mutate(city = gsub('Postdam', 'Potsdam', city)) %>% 
  mutate(city = gsub('s. l.', NA, city)) %>% 
  mutate(city = gsub('A többi adat hiányzik', NA, city)) %>% 
  mutate(city = gsub('Adatai ismeretlenek előttem', NA, city)) %>% 
  mutate(city = gsub('Közelebbi adatok még hiányoznak', NA, city)) %>% 
  mutate(city = gsub('Közelebbi adatok nélkül', NA, city)) %>% 
  mutate(city = gsub('További adatok még hiányoznak', NA, city)) %>% 
  
  mutate(year = gsub('A többi adat hiányzik', NA, year)) %>% 
  mutate(year = gsub('Adatai ismeretlenek előttem', NA, year)) %>% 
  mutate(year = gsub('Közelebbi adatok még hiányoznak', NA, year)) %>% 
  mutate(year = gsub('Közelebbi adatok nélkül', NA, year)) %>% 
  mutate(year = gsub('További adatok még hiányoznak', NA, year)) %>%
  
  left_join(cities, by = c("city" = "source")) %>%
  mutate(
    normalized_city = ifelse(
      is.na(normalized_city),
      city,
      normalized_city)
    ) %>%
  mutate(normalized_city = gsub('^s. l.$', NA, normalized_city)) %>% 

  # filter('Neuausgabe' == normalized_city |
  #        'Övergedrukt uit de "Gids"' == normalized_city) %>%
  # view()
  
  mutate(year_n = ifelse(is.na(year), year, as.integer(year))) %>%
  select(-is_series) %>% 
  write_csv('data/demeter.csv')

rm(list = ls())
