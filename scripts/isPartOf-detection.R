library(tidyverse)

df <- read_csv('data/demeter.csv', col_types = 'dcccccccccccccdl')
df.isPartOf <- read_csv('data/isPartOf.csv')

df %>% 
  filter(!is.na(isPartOf)) %>% 
  filter(is_container == FALSE) %>% 
  count()

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
  select(nyelv, normalized_city, year_n, szerzo) %>% 
  distinct() %>% 
  group_by(nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>% 
  filter(
    is.na(isPartOf)
    #& szerzo == 'JÓZSEF Attila'
  ) %>% 
  group_by(szerzo, nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

forrasok <- c('Források:', 'Források, antológiák', 'Antológiák – Források:', 'Források. Antológiák',
              'FORRÁSOK − ANTOLOGIÁK.')
.city <- 'Martin'
.year <- 1942
df %>%
  filter(
    !is.na(id)
    & magyar_cim %in% forrasok & id > 80
    & grepl(.city, normalized_city) & year_n == .year
  ) %>%
  select(id, szerzo, nyelv, idegen_cim, fordito, megjelenes) %>% 
  view()

.id <- 15226
df.filtered <- df %>% 
  filter(
    is.na(isPartOf)
    & grepl(.city, normalized_city) & year_n == .year
    #& grepl('Hviezdoslalov', megjelenes, ignore.case = TRUE)
    #& !id %in% c(2473, 15218, 33951)
    & id != .id
    #& nyelv == 'német'
    #& szerzo == 'BARTÓK Lajos'
    #& grepl('Gedichte', idegen_cim, ignore.case = TRUE)
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
