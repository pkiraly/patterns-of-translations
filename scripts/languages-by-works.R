library(tidyverse)
library(ggplot2)
library("stringdist")
source("scripts/functions.R")

df <- read_csv('data/demeter.csv', col_types = 'dccccccccccccd')

df %>%
  filter(magyar_cim != 'Források:' & szerzo != '---') %>% 
  select(szerzo, magyar_cim) %>%
  group_by(szerzo, magyar_cim) %>%
  count() %>%
  rename(count = n) %>% 
  filter(szerzo != 'PETŐFI Sándor') %>% # PETŐFI Sándor 
  filter(szerzo != 'VÖRÖSMARTY Mihály') %>% #  
  filter(szerzo != 'MADÁCH Imre') %>% #  
  filter(szerzo != 'MOLNÁR Ferenc') %>% #  
  filter(szerzo != 'ADY Endre') %>% #  
  filter(szerzo != 'ARANY János') %>% #  
  filter(szerzo != 'MIKSZÁTH Kálmán') %>% #  
  filter(szerzo != 'JÓKAI Mór') %>% #  
  filter(szerzo == 'JÓZSEF Attila') %>% # , 
  arrange(magyar_cim, desc(count)) %>%
  view()

# frequency of languages

df %>%
  filter(magyar_cim != 'Források:' & szerzo != '---') %>% 
  select(szerzo, magyar_cim, nyelv) %>%
  group_by(szerzo, magyar_cim, nyelv) %>%
  count() %>%
  rename(count = n) %>% 
  # filter(szerzo != 'PETŐFI Sándor') %>% 
  arrange(magyar_cim, desc(count)) %>%
  write_csv('data/languages-by-works.csv') %>% 
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

