library(tidyverse)
library(ggplot2)
library("stringdist")
source("scripts/functions.R")

df <- read_csv('data_raw/demeter.csv')

# frequency of languages
df %>%
  select(nyelv) %>%
  group_by(nyelv) %>%
  count() %>%
  rename(count = n) %>% 
  arrange(desc(count)) %>%
  write_csv('data/languages-by-publications.csv') %>% 
  view()

df %>%
  select(nyelv) %>%
  group_by(nyelv) %>%
  count() %>%
  rename(count = n) %>% 
  arrange(count) %>%
  #filter(count > 100) %>% 
  ggplot(aes(y = reorder(nyelv, count), x = count)) +
    geom_bar(stat = 'identity') +
    ylab("language") +
    xlab("publications")

