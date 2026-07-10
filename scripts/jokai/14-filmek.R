library(tidyverse)

df <- readRDS('data_raw/jokai.rds')
df
df2 <- df %>% 
  select(title = orig_title, lang = targ_lan_n, year = year_n, country, world) %>% 
  filter((year >= 1949 & year <= 1989) & !is.na(world) & country != 'Hungary') %>% 
  filter(world == 2)
df2

df2 %>% count(title) %>% arrange(desc(n)) %>% print(n = Inf)

df2 %>% 
  filter(title == 'Szegény gazdagok')

langs <- c('bolgár', 'lengyel', 'román', 'szlovák', 'orosz', 'német', 'cseh')

filmek <- read_csv('data_raw/jokai/filmek.csv') %>% 
  rename(movie_year = year) %>% 
  mutate(
    popularity = row_number()
  )
filmek

df3 <- df2 %>% 
  left_join(filmek, by = join_by(title)) %>% 
  filter(!is.na(movie_year)) %>% 
  filter(lang %in% langs) %>% 
  group_by(title, lang) %>% 
  mutate(first = year == min(year)) %>% 
  ungroup() %>% 
  filter(first == TRUE) %>% 
  mutate(
    d = year - movie_year,
    p = d > 0,
    k = popularity <= 5,
    title = sprintf('%s (%s)', title, movie_year),
  ) %>% 
  select(title, lang, d, p, k) %>% 
  distinct()

df3 %>% 
  filter(title == 'Az arany ember')

df3 %>% 
  ggplot(aes(x = d, y = lang, fill = p)) +
  geom_point() +
  geom_col(alpha=0.5) +
  facet_wrap(facets = vars(title), ncol = 2) + # cols = vars(k)
  theme_bw() +
  labs(
    x = 'a könyv és a film megjelenésének különbsége (év)',
    y = 'fordítás nyelve'
  ) +
  scale_fill_discrete(guide="none")

ggsave(
  'images/jokai/socialist-canon-vs-movies.png',
  width = 6, height = 5,
  units = 'in', dpi = 300)
