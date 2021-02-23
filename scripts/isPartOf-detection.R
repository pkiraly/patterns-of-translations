library(tidyverse)
library(ggplot2)
library(stringdist)

df <- read_csv('data/demeter.csv', col_types = 'dcccccccccccccd')
df.isPartOf <- read_csv('data/isPartOf.csv')

df %>% 
  filter(!is.na(isPartOf)) %>% 
  count()

df %>% 
  filter(
    is.na(isPartOf)
    & szerzo == 'PETŐFI Sándor'
  ) %>% 
  group_by(nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>%
  filter(
    !is.na(id)
    & magyar_cim == 'Források:' & id > 80
    & grepl('Milano', normalized_city)
    & is.na(year_n)
    # & id == 56908
    # grepl('FOOT PRINTS', idegen_cim)
  ) %>%
  select(id, szerzo, nyelv, idegen_cim, fordito, megjelenes) %>% 
  view()

df.filtered <- df %>% 
  filter(
    is.na(isPartOf)
    & grepl('Milano', normalized_city)
    & year == '191?'
    # & grepl('Poètes Hongrois', idegen_cim, ignore.case = TRUE)
    & grepl('P. Canti', megjelenes, ignore.case = TRUE)
    & id != 15080
    # & id != 71387  
    # & nyelv == 'francia'
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
  mutate(isPartOf2 = 15080) %>%
  union(df.isPartOf) %>%
  distinct() %>%
  write_csv('data/isPartOf.csv')

df.isPartOf <- read_csv('data/isPartOf.csv')
count(df.isPartOf)
