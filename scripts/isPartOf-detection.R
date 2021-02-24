library(tidyverse)

df <- read_csv('data/demeter.csv', col_types = 'dcccccccccccccd')
df.isPartOf <- read_csv('data/isPartOf.csv')

df %>% 
  filter(!is.na(isPartOf)) %>% 
  count()

df %>% 
  filter(is.na(isPartOf)) %>% 
  group_by(szerzo) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()
    
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
    & grepl('Paris', normalized_city)
    & year_n == 1871
  ) %>%
  select(id, szerzo, nyelv, idegen_cim, fordito, megjelenes) %>% 
  view()

df.filtered <- df %>% 
  filter(
    is.na(isPartOf)
    & grepl('Paris', normalized_city)
    & year_n == 1871
    # & grepl('Básn.', idegen_cim, ignore.case = TRUE)
    # & !grepl('Poésies', megjelenes, ignore.case = TRUE)
    & id != 14868
    # & nyelv == 'eszperantó'
    # & szerzo == 'PETŐFI Sándor'
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
  mutate(isPartOf2 = 14868) %>%
  union(df.isPartOf) %>%
  distinct() %>%
  write_csv('data/isPartOf.csv')

df.isPartOf <- read_csv('data/isPartOf.csv')
count(df.isPartOf)
