library(tidyverse)
# library(paletteer)
library(RColorBrewer)

df <- readRDS('data_raw/jokai.rds')
df
names(df)

df %>% 
  count(orig_title, orig_pub_yr) %>%
  filter(is.na(orig_pub_yr)) %>% 
  arrange(n) %>% 
  print(n = Inf)

df %>% 
  count(orig_title, orig_pub_yr) %>%
  # arrange(n) %>% 
  print(n = Inf)


df %>% 
  select(id, targ_lan_n, target_title, orig_title, orig_pub_yr, year_n, city_n, isbn) %>% 
  arrange(targ_lan_n, orig_title, year_n) %>% 
  select(targ_lan_n, orig_title, orig_pub_yr, year_n, city_n) %>% 
  print(n = Inf)

df_magyar <- read_csv('~/Documents/research/kisery/krasznahorkai-magyarul.csv')
df_magyar <- df_magyar %>% 
  mutate(title = ifelse(
    title == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
    "Északról hegy...",
    title)) %>% 
  rename(year_orig = year)
df_magyar

df_english <- read_csv('~/Documents/research/kisery/krasznahorkai-english-titles.csv')
df_english <- df_english %>% 
  mutate(
    hungarian = ifelse(
      hungarian == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
      "Északról hegy...",
      hungarian),
    english = ifelse(
      english == "A Mountain to the North, a Lake to the South, Paths to the West, a River to the East",
      "A Mountain to the North, ...",
      english),
    english = ifelse(
      english == "Destruction and Sorrow beneath the Heavens",
      "Destruction and Sorrow ...",
      english),
  )
