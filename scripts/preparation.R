library(tidyverse)
library(ggplot2)
library(stringdist)
source("scripts/functions.R")

df <- read_csv('data_raw/demeter.csv')
cities <- read_csv('data_raw/normalized-cities.csv')

df.normalized <- df %>% 
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
  select(-is_series)

df.isPartOf <- read_csv('data/isPartOf.csv')

df.connected <- df.normalized %>% 
  left_join(df.isPartOf, by='id') %>% 
  mutate(
    isPartOf = ifelse(
      !is.na(isPartOf),
      isPartOf,
      ifelse(
        !is.na(isPartOf2),
        isPartOf2,
        NA
      )
    )
  ) %>% 
  select(-isPartOf2)

df.containers <- df.connected %>% 
  select(isPartOf) %>% 
  filter(!is.na(isPartOf)) %>% 
  distinct() %>% 
  rename(id = isPartOf) %>% 
  mutate(is_container = TRUE) %>% 
  view()

df.with_containers <- df.connected %>%
  left_join(df.containers, by = 'id') %>% 
  mutate(is_container = ifelse(is.na(is_container), FALSE, is_container))
  
#'
#' processing data_raw/identical-ids.csv
#'
df.identical_raw <- read_csv('data_raw/identical-ids.csv')
df.identical <- df.identical_raw %>% 
  select(ids) %>% 
  separate(ids, c("A","B","C","D","E","F"), sep = ';', fill="right") %>% 
  gather("source", 'id') %>% 
  mutate(id = as.integer(id)) %>% 
  filter(!is.na(id))

false_duplicates <- df.identical %>% 
  filter(source != 'A') %>% 
  left_join(df.isPartOf, by = c('id' = 'isPartOf2'), keep = TRUE) %>% 
  filter(!is.na(isPartOf2)) %>% 
  select(source, id.x) %>% 
  distinct() %>% 
  rename('id' = 'id.x') %>% 
  select(id) %>% 
  unlist(use.names = FALSE)

if (length(false_duplicates) > 0) {
  print('false duplicates in data_raw/identical-ids.csv')
  print(false_duplicates)
}

true_duplicates1 <- df.identical %>% 
  filter(! id %in% false_duplicates) %>% 
  select(id) %>% 
  unlist(use.names = FALSE)

#'
#' processing data/duplications.csv
#' 
df.identical_raw <- read_csv('data/duplications.csv')
df.identical <- df.identical_raw %>% 
  select(ids) %>% 
  separate(ids, c("A","B","C","D","E","F"), sep = ';', fill="right") %>% 
  gather("source", 'id') %>% 
  mutate(id = as.integer(id)) %>% 
  filter(!is.na(id))

false_duplicates <- df.identical %>% 
  filter(source != 'A') %>% 
  left_join(df.isPartOf, by = c('id' = 'isPartOf2'), keep = TRUE) %>% 
  filter(!is.na(isPartOf2)) %>% 
  select(source, id.x) %>% 
  distinct() %>% 
  rename('id' = 'id.x') %>% 
  select(id) %>% 
  unlist(use.names = FALSE)

if (length(false_duplicates) > 0) {
  print('false duplicates in data/duplications.csv')
  print(false_duplicates)
}

true_duplicates2 <- df.identical %>% 
  filter(! id %in% false_duplicates) %>% 
  select(id) %>% 
  unlist(use.names = FALSE)

true_duplicates = c(true_duplicates1, true_duplicates2)

paste('removing', length(true_duplicates), 'duplicated items')

df.with_containers %>% 
  filter(!id %in% true_duplicates) %>% 
  write_csv('data/demeter.csv')

rm(list = ls())
