library(tidyverse)

df <- read_csv('data/demeter.csv', col_types = 'dcccccccccccccdl')
df.isPartOf <- read_csv('data/isPartOf.csv')

df %>% 
  filter(!is.na(isPartOf)) %>% 
  filter(is_container == FALSE) %>% 
  count()

df %>% 
  select(isPartOf) %>% 
  filter(!is.na(isPartOf)) %>% 
  group_by(isPartOf) %>% 
  count() %>% 
  view()
  
df %>% 
  filter(is.na(isPartOf)) %>% 
  filter(is_container == FALSE) %>% 
  group_by(szerzo) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>% 
  filter(is.na(isPartOf)) %>% 
  filter(is_container == FALSE) %>% 
  #filter(nyelv == 'angol') %>% 
  filter(normalized_city != 'Budapest') %>% 
  select(nyelv, normalized_city, year_n, szerzo) %>% 
  distinct() %>% 
  group_by(nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>% 
  filter(
    is.na(isPartOf)
    & !is.na(year_n)
    & !is.na(normalized_city)
    #& szerzo == 'MIKSZÁTH Kálmán'
    & grepl('"', megjelenes)
  ) %>% 
  group_by(szerzo, nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

forrasok <- c('Források:', 'Források, antológiák', 'Antológiák – Források:', 'Források. Antológiák',
              'FORRÁSOK − ANTOLOGIÁK.')
.city <- 'Leipzig'
.year <- 1972
df %>%
  filter(
    !is.na(id)
    & magyar_cim %in% forrasok & id > 80
    & grepl(.city, normalized_city) & year_n == .year
  ) %>%
  select(id, szerzo, nyelv, idegen_cim, fordito, megjelenes) %>% 
  view()

.id <- 64107
df.filtered <- df %>% 
  filter(
    !is.na(id)
    & is.na(isPartOf)
    & grepl(.city, normalized_city) & year_n == .year
    & grepl('Lügner', megjelenes, ignore.case = TRUE)
    #& !id %in% c(52358)
    & id != .id
    #& nyelv == 'udmurt'
    #& szerzo == 'MÓRICZ Zsigmond'
    #& grepl('Albumis', idegen_cim, ignore.case = TRUE)
  ) %>%
  view()

print(count(df.filtered))
.id
df.filtered %>% 
  select(id) %>%
  mutate(isPartOf2 = .id) %>%
  union(df.isPartOf) %>%
  distinct() %>% 
  write_csv('data/isPartOf.csv')

# df.isPartOf <- read_csv('data/isPartOf.csv')
# count(df.isPartOf)

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
