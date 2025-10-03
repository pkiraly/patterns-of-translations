#' filter the works of Jókai, creates
#' data_raw/jokai.rds

library(tidyverse)
# library(paletteer)
library(RColorBrewer)

df <- readRDS('data_raw/merged-Demeter-and-IT-(books-only)-v4-2025-09-23.rds')

postwar <- df %>% 
  # filter(year_n >= 1947 & year_n <= 1989) %>% 
  # filter(world == 2) %>% 
  mutate(
    country = ifelse(country == 'Czechoslovakia (to 1992)',
                     'Czechoslovakia', country),
    country = ifelse(country == 'German Democratic Rep. (to 1990)',
                     'GDR', country),
    country = ifelse(country == 'Germany',
                     'GDR', country),
    country = ifelse(country == 'German Democratic Republic',
                     'GDR', country),
    country = ifelse(country == 'USSR (to 1991)',
                     'USSR', country),
    country = ifelse(country == 'Soviet Union',
                     'USSR', country),
    country = ifelse(country == 'Democratic Republic of Vietnam (1954-)',
                     'Vietnam', country),
  ) %>% 
  mutate(region = 
           ifelse(country %in% c('Czechoslovakia', 'USSR'), 
                  paste0(country, '-', targ_lan_n), 
                  country)) %>% 
  #' remove filters
  filter(
    !(genre %in% c("báb\ngyerek", "gyerek", "gyerek\nnépmese", "népkölt", "népmese"))
  ) %>% 
  filter(grepl('JÓKAI Mór', author)) %>% 
  # count(author) %>% 
  saveRDS('data_raw/jokai.rds')

jokai <- read_csv('data_raw/jokai/jokai-magyarul.csv')

jokai %>% count(genre)
