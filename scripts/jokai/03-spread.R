library(tidyverse)
# library(paletteer)
library(RColorBrewer)
source('scripts/jokai/common-functions.R')

df <- readRDS('data_raw/jokai.rds')

df2 <- df %>% 
  select(lang = targ_lan_n, start = year_n, country) %>% 
  filter(!is.na(lang) & !is.na(start)) %>% 
  mutate(
    lang = case_match(
      lang,
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
      'szlovák' ~ 'Slovakian',
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
      'szlovén' ~ 'Slovanian',
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
    lang = ifelse(between(start, 1945, 1989)
                  & lang == 'German' 
                  & !is.na(country) & country == 'GDR', 
                  paste0('DDR-', lang), lang),
    lang = ifelse(lang == 'German' 
                  & !is.na(country) 
                  & country == 'Hungary',
                  paste0('HU-', lang), lang),
    lang = ifelse(
      lang == 'English',
      ifelse(!is.na(country) & country == 'UK', paste0('UK-', lang),
        ifelse(!is.na(country) & country == 'USA', paste0('US-', lang),
          ifelse(!is.na(country) & country == 'Hungary', paste0('HU-', lang),
            lang
          )
        )
      ),
      lang
    )
  )

langs <- df2 %>% 
  count(lang) %>% 
  arrange(desc(n), lang) %>% 
  select(lang) %>% 
  pull()

langs
year_range <- 3

df2 %>% 
  mutate(end = (start + year_range - 1)) %>% 
  rowwise() %>% 
  mutate(st = paste0(seq(start, end), collapse = "|")) %>% 
  ungroup() %>% 
  separate_wider_delim(st, "|", names=LETTERS[1:year_range], too_few = "align_start") %>% 
  select(-c(start, end)) %>% 
  pivot_longer(LETTERS[1:year_range], values_to = 'year') %>% 
  select(-name) %>% 
  mutate(year = as.integer(year)) %>% 
  count(lang, year) %>% 
  mutate(
    color = ifelse(grepl('German', lang),
                   'darkgreen',
                   ifelse(grepl('English', lang),
                          'darkblue',
                          'maroon')),
  ) %>% 
  ggplot(aes(x = year, y = factor(lang, levels = langs))) +
    geom_point(aes(size = n, color = color), alpha = 0.4) + #, color = 'maroon') +
    geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
    annotate("text", x = 1907, y = length(langs), label = "Jókai's death",
           color="cornflowerblue", hjust = "right", size = 8/.pt) +
    geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
    annotate("text", x = 1947, y = length(langs), label = "1945",
           color="cornflowerblue", hjust = "right", size = 8/.pt) +
    geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
    annotate("text", x = 1991, y = length(langs), label = "1989",
           color="cornflowerblue", hjust = "right", size = 8/.pt) +
    labs(
      x = 'publication year', # 'kiadási év',
      y = 'language (in order of total publications)', # 'nyelv (az összes megjelenés sorrendjében)',
      title = 'Jókai\'s presence at the book market', # 'Jókai jelenléte a könyvpiacon',
      subtitle = sprintf('presence at the book market = publication + %d years', year_range),
      # sprintf('könyvpiaci jelenlét = megjelenés + %d év', year_range),
      size = '', # 'egyszerre\njelenlevő\nművek\nszáma'
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1)) +
    scale_color_identity() +
    coord_flip()


ggsave('images/jokai/language-marketplace.png',
       width = 8, height = calculateHeight(8), units = 'in', dpi = 300)

quit('no')
#'----

columns <- paste('a', 1:year_range, sep='')

df2 %>% 
  mutate(end = (start + year_range - 1)) %>% 
  rowwise() %>% 
  mutate(st = paste0(seq(start, end), collapse = "|")) %>% 
  ungroup() %>% 
  separate_wider_delim(st, "|", names=columns, too_few = "align_start") %>% 
  select(-c(start, end)) %>% 
  pivot_longer(-lang, values_to = 'year', names_to = 'weight') %>% 
  mutate(
    year = as.integer(year),
    weight = (year_range - (as.numeric(str_remove(weight, "a")) - 1)) / year_range
  ) %>% 
  group_by(lang, year) %>% 
  summarise(n = sum(weight)) %>% 
  ungroup() %>% 
  mutate(
    langs_factor = factor(lang, levels = rev(langs)),
    langs_rank = as.integer(langs_factor)
  ) %>% 
  ggplot(aes(x = year, y = langs_rank)) +
    geom_point(aes(size = n), alpha = 0.4, color = 'maroon') +
    geom_point(aes(size = n/2, y = langs_rank + 0.4),
             alpha = 0.4, color = 'green') +
    geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
    annotate(
      "text", x = 1906, y = 1, label = "Jókai halála",
      color="cornflowerblue", hjust = "left", size = 8/.pt) +
    geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
    annotate(
      "text", x = 1946, y = 1, label = "1945",
      color="cornflowerblue", hjust = "left", size = 8/.pt) +
    geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
    annotate(
      "text", x = 1990, y = 1, label = "1989",
      color="cornflowerblue", hjust = "left", size = 8/.pt) +
    theme_bw() +
    labs(
      x = 'kiadási év',
      y = 'nyelv (az összes megjelenés sorrendjében)',
      title = 'Jókai jelenléte a könyvpiacon',
      subtitle = sprintf('könyvpiaci jelenlét = megjelenés + %d év', year_range),
      size = 'egyszerre\njelenlevő\nművek\nszáma'
    ) +
    scale_y_continuous(
      breaks = length(langs):1,
      labels = langs,
      # minor_breaks = seq(1850, 1900, 1)
    )

create_year_range <- function(year_range) {
  print(year_range)
  columns <- paste('a', 1:year_range, sep='')
  print(columns)

  df2 %>% 
    mutate(end = (start + year_range - 1)) %>% 
    rowwise() %>% 
    mutate(st = paste0(seq(start, end), collapse = "|")) %>% 
    ungroup() %>% 
    separate_wider_delim(st, "|", names=columns, too_few = "align_start") %>% 
    select(-c(start, end)) %>% 
    pivot_longer(-lang, values_to = 'year', names_to = 'weight') %>% 
    mutate(
      year = as.integer(year),
      weight = as.numeric(str_remove(weight, "a")),
      weight = (year_range + 1 - weight) / year_range,
      # weight = 1 / weight
    ) %>% 
    group_by(lang, year) %>% 
    summarise(n = sum(weight)) %>% 
    ungroup() %>% 
    mutate(normalized = n / max(n))
}

df2
df03 <- create_year_range(3)
df10 <- create_year_range(10)

alpha_scale <- 5
df3 <- df03 %>% 
  full_join(df10, by = join_by(lang, year), # suffix = c('03', '10')
  ) %>% 
  rename(
    v03_n = n.x, v03_r = normalized.x,
    v10_n = n.y, v10_r = normalized.y,
  ) %>% 
  mutate(
    langs_rank = as.integer(factor(lang, levels = rev(langs)))
  ) %>% 
  pivot_longer(-c(lang, year, langs_rank), 
               names_to = c("name", ".value"),
               names_sep = '_') %>% 
  mutate(
    name = ifelse(name == 'v03', '3 év', '10 év'),
    name = factor(name, levels = c('3 év', '10 év')),
    y = langs_rank + r,
    alpha = (alpha_scale+r)/(alpha_scale+1)
  )
df3 %>% arrange(desc(n))

df3 %>% 
  ggplot(aes(x = year, y = langs_rank)) +
  geom_point(aes(y = y, alpha = alpha), color = 'maroon', size = 0.1) +
  geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1906, y = 1, label = "Jókai halála",
    color="cornflowerblue", hjust = "left", size = 8/.pt) +
  geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1946, y = 1, label = "1945",
    color="cornflowerblue", hjust = "left", size = 8/.pt) +
  geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1990, y = 1, label = "1989",
    color="cornflowerblue", hjust = "left", size = 8/.pt) +
  facet_wrap(vars(name)) +
  theme_bw() +
  labs(
    x = 'kiadási év',
    y = 'nyelv (a kiadásszámok sorrendjében)',
    title = 'Jókai jelenléte a könyvpiacon',
    subtitle = sprintf('különböző hosszúságú és intenzitású piaci jelenlétet feltételezve', year_range),
    size = 'egyszerre\njelenlevő\nművek\nszáma'
  ) +
  scale_y_continuous(
    breaks = length(langs):1,
    labels = langs,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  scale_size_area() +
  scale_alpha_identity()
