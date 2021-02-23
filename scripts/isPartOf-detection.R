library(tidyverse)
library(ggplot2)
library(stringdist)
source("scripts/functions.R")

df <- read_csv('data/demeter.csv', col_types = 'dcccccccccccccd')
df.isPartOf <- read_csv('data/isPartOf.csv')

df %>% 
  filter(!is.na(isPartOf)) %>% 
  count()

df %>% 
  filter(is.na(isPartOf)) %>% 
  group_by(nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>%
  filter(
    magyar_cim == 'Források:' & id > 80
    & grepl('Budapest', normalized_city)
    & year == '1963'
    # id == 15079
    # grepl('FOOT PRINTS', idegen_cim)
  ) %>%
  select(id, szerzo, nyelv, idegen_cim, fordito, megjelenes) %>% 
  view()

# 85
df.filtered <- df %>% 
  filter(
    is.na(isPartOf)
    & grepl('Budapest', normalized_city)
    & year_n == 1963
    # & !grepl('Cобрание', megjelenes, ignore.case = TRUE)
    & id != 35021
    & nyelv == 'orosz'
    & szerzo == 'PETŐFI Sándor'
    # & grepl('Pongrácz', fordito)
  ) %>% 
  # ---------
  # mutate(megjelenes = gsub(' (стр|Стр|cтр)\\. \\d+\\.?$', '', megjelenes)) %>%
  # select(megjelenes) %>%
  # group_by(megjelenes) %>%
  # count() %>%
  # select(megjelenes) %>%
  # ----------
  # select(fordito) %>%
  # group_by(fordito) %>%
  # count() %>%
  # select(fordito) %>%
  view()

df.filtered %>% 
  select(id) %>%
  mutate(isPartOf2 = 35021) %>%
  union(df.isPartOf) %>%
  distinct() %>%
  write_csv('data/isPartOf.csv') %>%
  view()

df.isPartOf <- read_csv('data/isPartOf.csv')
count(df.isPartOf)
