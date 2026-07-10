library(tidyverse)
# library(paletteer)
library(RColorBrewer)
library(colorspace)
pdf.options(encoding = 'CP1250')
source('scripts/jokai/common-functions.R')

df <- readRDS('data_raw/jokai.rds')
df
df <- df %>% 
  mutate(
    targ_lan_n = case_match(
      targ_lan_n,
      'német' ~ 'German',
      'angol' ~ 'English',
      'cseh' ~ 'Czech',
      'észt' ~ 'Estonian',
      'finn' ~ 'Finnish',
      'francia' ~ 'French',
      'lengyel' ~ 'Polish',
      'olasz' ~ 'Italian',
      'svéd' ~ 'Swedish',
      'szerb' ~ 'Serbian',
      'szlovák' ~ 'Slovak',
      'bolgár' ~ 'Bulgarian',
      'latin' ~ 'Latin',
      'dán' ~ 'Danish',
      'orosz' ~ 'Russian',
      'horvát' ~ 'Croatian',
      'spanyol' ~ 'Spanish',
      'román' ~ 'Romanian',
      'örmény' ~ 'Armenian',
      'török' ~ 'Turkish',
      'holland' ~ 'Dutch',
      'kínai' ~ 'Chinese',
      'ukrán' ~ 'Ukranian',
      'szlovén' ~ 'Slovenian',
      'héber' ~ 'Hebrew',
      'grúz' ~ 'Georgian',
      'eszperantó' ~ 'Esperanto',
      'görög' ~ 'Greek',
      'lett' ~ 'Latvian',
      'rutén' ~ 'Ruthenian',
      'litván' ~ 'Lithuanian',
      'azerbajdzsán' ~ 'Azerbaijani',
      'tadzsik' ~ 'Tajik',
      'türkmén' ~ 'Turkmen',
      'katalán' ~ 'Catalan',
      'beás romani' ~ 'Boyash romani',
      'vietnámi' ~ 'Vietnamese',
    )
  ) %>% 
  mutate(
    targ_lan_n = ifelse(
      (between(year_n, 1945, 1989)
        & targ_lan_n == 'German' 
        & !is.na(country) & country == 'GDR'), 
      paste0('DDR\n', targ_lan_n),
      targ_lan_n),
    targ_lan_n = ifelse(
      (targ_lan_n == 'German' 
        & !is.na(country) 
        & country == 'Hungary'),
      paste0('HU\n', targ_lan_n),
      targ_lan_n),
    targ_lan_n = ifelse(
      targ_lan_n == 'English',
      ifelse(!is.na(country) & country == 'UK',
             paste0('UK\n', targ_lan_n),
             ifelse(!is.na(country) & country == 'USA',
                    paste0('US\n', targ_lan_n),
                    ifelse(!is.na(country) & country == 'Hungary',
                           paste0('HU\n', targ_lan_n),
                           targ_lan_n))),
      targ_lan_n
    )
  )

df %>% count(genre)
df %>% 
  filter(is.na(genre) & orig_title != 'Források:') %>% 
  select(orig_title)

genres <- read_csv('data_raw/jokai/jokai-magyarul.csv')
genres
df %>% select(title = orig_title, genre2 = genre) %>% 
  filter(title != "Források:") %>% 
  distinct() %>% 
  left_join(genres, by = join_by(title == title)) %>% 
  filter(is.na(genre) && is.na(genre2))

df %>% 
  select(orig_title) %>% 
  filter(grepl(' − ', orig_title)) %>% 
  count(orig_title) %>% 
  arrange(desc(n))

selected_titles <- df %>%
  filter(!is.na(targ_lan_n) & !is.na(orig_title)) %>%
  filter(!(orig_title %in% c('Források:'))) %>% 
  select(orig_title, targ_lan_n) %>% 
  distinct() %>% 
  group_by(orig_title) %>% 
  summarise(c = n()) %>% 
  filter(c > 3) %>% 
  select(orig_title) %>% pull()

selected_titles

df2 <- df %>%
  # filter(orig_title %in% selected_titles) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  mutate(
    title = sprintf("%s (%d)", orig_title, orig_pub_yr)
  ) %>% 
  arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
  select(orig_title, orig_pub_yr, targ_lan_n, year_n)

df2
df2 %>% 
  count(orig_pub_yr, year_n) %>% 
  ggplot(aes(x = year_n, y = orig_pub_yr, size=n)) +
  geom_point(alpha=0.5, color = '#047857') +
  geom_abline(color = "#cccccc") +
  theme_bw() +
  labs(
    x = 'publication of the translation',
    y = 'publication of the original work'
  ) +
  scale_y_continuous(
    breaks = seq(1850, 1905, 5),
    limits = c(1850, 1905)
    ) +
  scale_x_continuous(
    breaks = seq(1850, 2010, 10),
    limits = c(1850, 2010)) +
  scale_size_continuous(guide="none")
ggsave('images/jokai/work-timeline-simplified.png',
       width = 12, height = calculateHeight(12), units = 'in', dpi = 300)

first_editions <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  # filter(orig_title %in% selected_titles) %>% 
  select(id, orig_title, orig_pub_yr, targ_lan_n, year_n) %>% 
  arrange(orig_title, targ_lan_n, year_n) %>% 
  group_by(orig_title, targ_lan_n) %>% 
  mutate(kiadas = row_number()) %>% 
  ungroup() %>% 
  filter(kiadas == 1) %>% 
  select(id) %>% 
  pull()

first_editions

df2 <- df %>%
  filter(id %in% first_editions) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  mutate(
    title = sprintf("%s (%d)", orig_title, orig_pub_yr)
  ) %>% 
  arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
  select(orig_title, orig_pub_yr, targ_lan_n, year_n)

df2
df2 %>% 
  count(orig_pub_yr, year_n) %>% 
  ggplot(aes(x = year_n, y = orig_pub_yr, size=n)) +
  geom_point(alpha=0.5, color = '#047857') +
  geom_abline(color = "#cccccc") +
  theme_bw() +
  labs(
    x = 'publication of the translation',
    y = 'publication of the original work'
  ) +
  scale_y_continuous(
    breaks = seq(1850, 1905, 5),
    limits = c(1850, 1905)
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2010, 10),
    limits = c(1850, 2010)) +
  scale_size_continuous(guide="none")
ggsave('images/jokai/work-timeline-simplified-first-editions.png',
       width = 12, height = calculateHeight(12), units = 'in', dpi = 300)


#' --- END
