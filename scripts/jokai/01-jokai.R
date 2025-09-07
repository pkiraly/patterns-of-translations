#' filter the works of Jókai, creates
#' data_raw/jokai.rds

library(tidyverse)
# library(paletteer)
library(RColorBrewer)

world2 <- readRDS('data_raw/regions.rds')
world2 %>% 
  filter(grepl('JÓKAI Mór', author)) %>% 
  # count(author) %>% 
  saveRDS('data_raw/jokai.rds')

jokai <- read_csv('data_raw/jokai/jokai-magyarul.csv')

jokai %>% count(genre)
