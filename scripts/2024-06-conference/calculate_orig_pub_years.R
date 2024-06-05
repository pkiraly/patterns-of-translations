#' calculate possible original publication year based on year of birth
#' 
library(tidyverse)

df <- readRDS('data_raw/postwar.rds')
world2 <- df %>% 
  filter(country != "Hungary")

world2_without_pubyr <- world2 %>% 
  filter(is.na(orig_pub_yr)) %>% 
  select(id, orig_pub_yr, author, year_n)

world2_without_pubyr %>% 
  count(author) %>% 
  arrange(desc(n)) %>% 
  print(n = 20)

auth <- read_csv('data_raw/authors-count-enhanced-v2.csv')
auth_flourish <- auth %>% 
  mutate(
    year_of_birth =  as.integer(str_sub(dateOfBirth, 1, 4)),
    flourish = year_of_birth + 25
  ) %>% 
  select(szerzo, year_of_birth, flourish)

calculated_pub_years <- world2_without_pubyr %>% 
  left_join(auth_flourish, by = join_by(author == szerzo)) %>% 
  filter(!is.na(year_of_birth)) %>% 
  mutate(
    calculated_pub_yr = flourish
  ) %>% 
  select(id, calculated_pub_yr)

write_csv(calculated_pub_years, 'data_raw/calculated_pub_years.csv')

calculated_pub_years %>% 
  ggplot(aes(x = calculated_pub_yr)) +
  geom_histogram(bins = 250)
