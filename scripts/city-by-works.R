library(tidyverse)
df <- read_csv('data/demeter.csv', col_types = 'dccccccccccccd')

df %>% 
  select(normalized_city) %>% 
  mutate(
    count = 1.0,
    cities = (str_count(normalized_city, '−') + 1) *  1.0) %>% 
  separate(normalized_city, c("A","B","C","D"), sep = '−', fill="right") %>% 
  gather("code", "city", -c(count, cities)) %>% 
  mutate(count = count/cities) %>% 
  select(count, city) %>% 
  group_by(city) %>% 
  summarise(works = sum(count)) %>%
  write_csv('data/city-by-works.csv') %>% 
  view()

df %>% 
  select(normalized_city, year_n, nyelv) %>% 
  mutate(
    count = 1.0,
    cities = (str_count(normalized_city, '−') + 1) *  1.0) %>% 
  separate(normalized_city, c("A","B","C","D"), sep = '−', fill="right") %>% 
  gather("code", "city", -c(year_n, nyelv, count, cities)) %>% 
  mutate(count = count/cities) %>% 
  select(city, year_n, nyelv, count) %>% 
  group_by(city, year_n, nyelv) %>% 
  summarise(works = sum(count)) %>%
  write_csv('data/city-year-language-works.csv') %>% 
  view()

df <- read_csv('data/city-year-language-works.csv')
df %>% 
  group_by(city, year_n) %>% 
  count() %>%
  rename(languages = n) %>% 
  write_csv('data/city-year-languages.csv') %>% 
  view()

df %>% 
  group_by(city) %>% 
  count() %>%
  rename(publications = n) %>% 
  write_csv('data/city-publications.csv') %>% 
  view()

df %>% 
  select(city, year_n) %>% 
  group_by(city) %>% 
  mutate(
    count = n(),
    min_year = min(year_n, na.rm = TRUE),
    max_year = max(year_n, na.rm = TRUE),
    span = max_year+1 - min_year,
    density = span / count
  ) %>% 
  ungroup() %>% 
  select(-year_n) %>% 
  distinct() %>% 
  # rename(publications = n) %>% 
  # write_csv('data/city-publications.csv') %>% 
  filter(!is.na(city) & city != 'Budapest' & city != 's. l'
         & count > 20) %>% 
  ggplot(aes(count, span, label = city)) +
    # geom_point(colour = 'maroon') +
    geom_text(colour = 'brown') +
    ggtitle('Publications and timespan') +
    ylab('time span in years') +
    xlab('minimum number of publications')

ggsave("images/publications-by-timespan.png", 
       width = 12, height = 6, units = 'in', dpi = 300)

# summarise(publications = sum(count)) %>%
# group_by(city, year_n, nyelv) %>% 
# count() %>% 

  
  select(count, city) %>% 
  group_by(city) %>% 
  summarise(works = sum(count)) %>%
  write_csv('data/city-by-works.csv') %>% 
  view()
