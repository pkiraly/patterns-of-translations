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
    #& szerzo == 'MAGYAR NÉPKÖLTÉS − VERS'
  ) %>% 
  group_by(szerzo, nyelv, normalized_city, year_n) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>%
  filter(
    !is.na(id)
    & magyar_cim == 'Források:' & id > 80
    & grepl('Moscow', normalized_city) & year_n == 1949
  ) %>%
  select(id, szerzo, nyelv, idegen_cim, fordito, megjelenes) %>% 
  view()

df.filtered <- df %>% 
  filter(
    is.na(isPartOf)
    & grepl('Moscow', normalized_city) & year_n == 1949
    # & grepl('Gedichte', megjelenes, ignore.case = TRUE)
    & id != 15132
    #& nyelv == 'francia'
    & szerzo == 'PETŐFI Sándor'
    #& grepl('Klänge', idegen_cim, ignore.case = TRUE)
  ) %>% 
  view()

print(count(df.filtered))

df.filtered %>% 
  select(id) %>%
  mutate(isPartOf2 = 15132) %>%
  union(df.isPartOf) %>%
  distinct() %>%
  write_csv('data/isPartOf.csv')

df.isPartOf <- read_csv('data/isPartOf.csv')
count(df.isPartOf)

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
