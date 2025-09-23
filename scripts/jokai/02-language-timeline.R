library(tidyverse)
# library(paletteer)
library(RColorBrewer)

df <- readRDS('data_raw/jokai.rds')
df
names(df)

df %>% 
  count(orig_title) %>% 
  filter(grepl('fia', orig_title))
  
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


languages <- df %>% 
  count(targ_lan_n) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 5) %>% 
  select(targ_lan_n) %>% 
  unlist(use.names = FALSE)
languages

df %>% 
  select(title, year_orig, genre) %>%
  distinct() %>% 
  mutate(title = sprintf("%s%s", title, 
                         ifelse(!is.na(genre) & genre == "regény", "*", "")))

names(df)
#' id, author, orig_title, genre, orig_pub_yr, orig_publ_city, targ_lan_n, country
#' world, target_title, translator, megjelenes, megjegyzes, city_n, year_n, isPartOf
#' HU-minor, interm_title, interm_lang, is_container, series, db, auth_quality, 
#' transl_quality, publisher, pagination, orig_lang, editionstat, isbn, kotet
#' city, year, targ_lan, region     

selected_titles <- df %>%
  filter(!is.na(targ_lan_n) & !is.na(orig_title)) %>%
  filter(!(orig_title %in% c('Források:'))) %>% 
  select(orig_title, targ_lan_n) %>% 
  distinct() %>% 
  group_by(orig_title) %>% 
  summarise(c = n()) %>% 
  filter(c > 4) %>% 
  select(orig_title) %>% pull()
selected_titles

df2 <- df %>%
  filter(orig_title %in% selected_titles) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  mutate(
    title = sprintf("%s (%d)", orig_title, orig_pub_yr)
  ) %>% 
  arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
  select(orig_title, orig_pub_yr, targ_lan_n, year_n)

multiyear <- df2 %>%
  select(orig_pub_yr, orig_title) %>% 
  distinct() %>% 
  group_by(orig_pub_yr) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

multiyear

df3 <- df2 %>%
  select(orig_pub_yr, orig_title) %>% 
  distinct() %>%
  arrange(orig_pub_yr) %>% 
  group_by(orig_pub_yr) %>% 
  mutate(
    id = row_number(), 
    n = n(),
    z = (id - 1) * (1 / n),
    y = orig_pub_yr + z,
    value = rnorm(n = 1),
    title = sprintf("%s (%s)", orig_title, orig_pub_yr)
  )

title_axis <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  select(orig_pub_yr, orig_title) %>% 
  distinct() %>% 
  group_by(orig_pub_yr) %>% 
  mutate(title = paste(orig_title, collapse = "\n")) %>% 
  distinct(orig_pub_yr, title) %>% 
  arrange(orig_pub_yr) %>%
  mutate(title = sprintf("%s (%d)", title, orig_pub_yr))

df3

df3 %>% 
  ggplot(aes(x = value, y = y)) + 
  geom_point() +
  scale_y_continuous(
    breaks = df3$y,
    labels = df3$title,
    # minor_breaks = seq(1840, 1900, 1)
  )
  
df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  count(targ_lan_n) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

language_colors <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  count(targ_lan_n) %>% 
  arrange(desc(n)) %>% 
  mutate(
    id = row_number(),
    color = ifelse(id < 8, targ_lan_n, 'egyéb')
  ) %>% 
  select(targ_lan_n, color)

language_colors

df %>%
  filter(orig_title %in% selected_titles) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  left_join(df3, by = join_by(orig_pub_yr, orig_title)) %>% 
  left_join(language_colors, by = join_by(targ_lan_n)) %>% 
  arrange(orig_pub_yr) %>% 
  # select(orig_pub_yr, orig_title, y) %>% 
  # arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
  ggplot(aes(x = year_n, y = y)) +
  geom_jitter(aes(colour = color), height = 0.1, alpha = 0.7) +
  geom_abline(color = "#cccccc") +
  geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1906, y = 1893.5, label = "Jókai halála",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1946, y = 1893.5, label = "1945",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1990, y = 1893.5, label = "1989",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  scale_y_continuous(
    breaks = title_axis$orig_pub_yr,
    labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
    ) +
  scale_x_continuous(
    breaks = seq(1850, 2030, 10),
    # labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  labs(
    title = "Jókai Mór műveinek fordításai",
    subtitle = "csak a legalább 4 nyelvre lefordított alkotások",
    x = "Az alkotás fordítása",
    y = "Alkotás",
    color = "languages",
    shape = "",
    caption = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(
    values = c(brewer.pal(7,"Set1"), '#DBC5A0FF')
  )

ggsave('images/jokai/work-timeline.png',
       width = 12, height = 8, units = 'in', dpi = 300)


#' ----

first_editions <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  select(id, orig_title, orig_pub_yr, targ_lan_n, year_n) %>% 
  arrange(orig_title, targ_lan_n, year_n) %>% 
  group_by(orig_title, targ_lan_n) %>% 
  mutate(kiadas = row_number()) %>% 
  ungroup() %>% 
  filter(kiadas == 1) %>% 
  select(id) %>% 
  pull()


first_editions

df %>%
  filter(orig_title %in% selected_titles) %>% 
  filter(id %in% first_editions) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  left_join(df3, by = join_by(orig_pub_yr, orig_title)) %>% 
  left_join(language_colors, by = join_by(targ_lan_n)) %>% 
  arrange(orig_pub_yr) %>% 
  # select(orig_pub_yr, orig_title, y) %>% 
  # arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
  ggplot(aes(x = year_n, y = y)) +
  geom_jitter(aes(colour = color), height = 0.1, alpha = 0.7) +
  geom_abline(color = "#cccccc") +
  geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1906, y = 1893.5, label = "Jókai halála",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1946, y = 1893.5, label = "1945",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1990, y = 1893.5, label = "1989",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  scale_y_continuous(
    breaks = title_axis$orig_pub_yr,
    labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2030, 10),
    # labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  labs(
    title = "Jókai Mór műveinek fordításai - első kiadások",
    subtitle = "csak a legalább 4 nyelvre lefordított alkotások",
    x = "Az alkotás fordítása",
    y = "Alkotás",
    color = "languages",
    shape = "",
    caption = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(
    values = c(brewer.pal(7,"Set1"), '#DBC5A0FF')
  )

ggsave('images/jokai/work-timeline-first-editions.png',
       width = 12, height = 8, units = 'in', dpi = 300)
