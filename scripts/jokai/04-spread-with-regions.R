library(tidyverse)

df <- readRDS('data_raw/jokai.rds')

east_german <- c('GDR') # 'Hungary'

names(df)
df2 <- df %>% 
  select(lang = targ_lan_n, start = year_n, country, region) %>% 
  filter(!is.na(lang) & !is.na(start)) %>% 
  mutate(
    lang = ifelse(between(start, 1945, 1989)
                  & lang == 'német' 
                  & !is.na(country) & country %in% east_german, 
         'DDR-német', lang),
    lang = ifelse(lang == 'német' 
                  & !is.na(country) 
                  & country == 'Hungary',
         'HU-német', lang),
    lang = ifelse(
      lang == 'angol',
      ifelse(
        !is.na(country) & country == 'UK',
        'UK-angol',
        ifelse(
          !is.na(country) & country == 'USA',
          'US-angol',
          'angol'
        )
      ),
      lang
    )
  ) %>% 
  select(lang, start) %>% 
  filter(!is.na(lang) & !is.na(start)) 
  
df %>% 
  # select(lang = targ_lan_n, start = year_n, country, region) %>% 
  filter(targ_lan_n == 'német' & is.na(country)) %>% 
  count(city_n, year_n) %>%
  group_by(city_n) %>% 
  summarise(
    min = min(year_n, na.rm = TRUE),
    max = max(year_n, na.rm = TRUE),
    n = sum(n)
  ) %>% 
  print(n = Inf)

df2 %>% 
  # filter(between(year_n, 1945, 1989)) %>% 
  filter(lang == 'HU-német') %>% 
  count(start) %>% 
  print(n = Inf)

df %>% 
  filter(is.na(country)) %>% 
  count(targ_lan_n) %>% 
  arrange(desc(n)) %>% 
  print(n = Inf)

df %>% 
  select(lang = targ_lan_n, start = year_n, country, region) %>% 
  filter(is.na(lang) | is.na(start)) %>% 
  print(n = Inf)

df2 %>% count(lang) %>% arrange(desc(n)) %>% print(n = Inf)

df2

langs <- df2 %>% 
  count(lang) %>% 
  arrange(desc(n), lang) %>% 
  select(lang) %>% 
  pull()


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

df03 <- create_year_range(3)
df06 <- create_year_range(6)
df10 <- create_year_range(10)

alpha_scale <- 5
df3 <- df03 %>% 
  full_join(df06, by = join_by(lang, year), suffix = c(".3", ".6")) %>% 
  full_join(df10, by = join_by(lang, year)) %>% 
  rename(
    v03_n = n.3, v03_r = normalized.3,
    v06_n = n.6, v06_r = normalized.6,
    v10_n = n,   v10_r = normalized,
  ) %>% 
  mutate(
    langs_rank = as.integer(factor(lang, levels = rev(langs)))
  ) %>% 
  pivot_longer(-c(lang, year, langs_rank), 
               names_to = c("name", ".value"),
               names_sep = '_') %>% 
  mutate(
    name = ifelse(name == 'v03', '3 év', (ifelse(name == 'v06', '6 év', '10 év'))),
    name = factor(name, levels = c('3 év', '6 év', '10 év')),
    y = langs_rank + r,
    alpha = (alpha_scale+r)/(alpha_scale+1),
    color = ifelse(grepl('német', lang),
                   'darkgreen',
                   ifelse(grepl('angol', lang),
                          'darkblue',
                          'maroon'))
  )
df3

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
    subtitle = 'különböző hosszúságú és intenzitású piaci jelenlétet feltételezve',
    size = 'egyszerre\njelenlevő\nművek\nszáma'
  ) +
  scale_y_continuous(
    breaks = length(langs):1,
    labels = langs,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  scale_size_area() +
  scale_alpha_identity()

df3
df3 %>% 
  ggplot(aes(x = year, y = langs_rank)) +
  # geom_step(aes(y = langs_rank + 0.4, group = langs_rank, size = r), color = 'green') + 
  # geom_path(aes(y = langs_rank + 0.4, group = langs_rank, size = r), color = 'green') + 
  geom_line(
    aes(
      group = langs_rank, size = r, 
      color = color
    ),
    show.legend = FALSE,
    
    ) + 
  # geom_line(aes(y = y, group = langs_rank), color = 'maroon', size = .5) + 
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
    subtitle = 'különböző hosszúságú és intenzitású piaci jelenlétet feltételezve (zöld: német régiók, kék: angol régiók)',
    size = NULL
  ) +
  scale_y_continuous(
    breaks = length(langs):1,
    labels = langs,
    # minor_breaks = seq(1850, 1900, 1)
  )+ 
  scale_size_area() +
  scale_color_identity()

ggsave('images/jokai/language-marketplace-extended-3-phase.png',
       width = 12, height = 12, units = 'in', dpi = 300)
