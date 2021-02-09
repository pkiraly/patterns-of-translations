library(tidyverse)
library(ggplot2)

df_lang <- read_csv('data_raw/languages.csv')

df %>% 
  arrange(desc(count))
