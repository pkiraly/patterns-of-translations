library(tidyverse)

df <- read_csv('data/demeter.csv', col_types = 'dcccccccccccccd')

df %>% 
  select(szerzo) %>% 
  filter(szerzo != '---') %>% 
  filter(!grepl('MAGYAR NÉPKÖLTÉS', szerzo)) %>% 
  filter(!grepl('ISMERETLEN Költő', szerzo)) %>% 
  mutate(
    szerzo = gsub('\\s*−\\s*', '−', szerzo)
  ) %>%
  separate(szerzo, c("A","B"), sep = '−', fill="right") %>% 
  gather('order', 'szerzo', na.rm = TRUE) %>% 
  select(szerzo) %>% 
  group_by(szerzo) %>% 
  count() %>%
  rename(count = n) %>% 
  # select(count, A, B) %>% 
  arrange(szerzo) %>% 
  # select(szerzo, count) %>% 
  write_csv('data/authors-count.csv')
