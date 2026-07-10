library(tidyverse)
library(stringr)
library(ggrepel)


canon <- tribble(
  ~title, ~count,
  'Sárga rózsa', 2,
  'Szegény gazdagok', 2,
  'Rab Ráby', 2,
  'Kárpáthy Zoltán', 3,
  'Janicsárok végnapjai', 3,
  'Fekete gyémántok', 2,
  'Egy magyar nábob', 4,
  'Az arany ember', 3,
  'A lőcsei fehér asszony', 3,
  'A kőszívű ember fiai', 3,
  'A kis királyok', 2
)
canonical <- canon %>% filter(count > 2) %>% pull(title)
canonical

ce <- c('cseh', 'szlovák', 'lengyel', 'német')

df <- readRDS('data_raw/jokai.rds')
df
df2 <- df %>% 
  select(title = orig_title, lang = targ_lan_n, year = year_n, country, world) %>% 
  filter((year >= 1949 & year <= 1989) & !is.na(world) & country != 'Hungary') %>% 
  mutate(
    canonical = ifelse(title %in% canonical, 'canonical', 'others'),
    world = ifelse(world != 2, 
                   world,
                   ifelse (lang %in% ce,
                           4,
                           world
                   )
    
    ),
    world = case_match(
      world,
      1 ~ 'West',
      2 ~ 'East',
      3 ~ '3rd world',
      4 ~ 'Central EU',
    )
  )


df2 %>% count(world, canonical) %>% 
  pivot_wider(id_cols = world,
              names_from = canonical,
              values_from = n,
              values_fill = 0) %>% 
  mutate(percent = canonical * 100 / (canonical + others))

df2 %>% 
  ggplot(aes(x = year, y = world, color=canonical)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.5) +
  theme_bw() +
  labs(
    x = 'publication year',
    y = 'region',
    color = NULL
  )
ggsave(
  'images/jokai/socialist-canon.png',
  width = 8, height = 4,
  units = 'in', dpi = 300)




df3 <- df2 %>% 
  filter(world %in% c('East', 'Central EU')) %>% 
  mutate(world = ifelse(world == 'Central EU', 'CEU', world)) %>% 
  count(title, world) %>% 
  pivot_wider(id_cols = title,
              names_from = world,
              values_from = n,
              values_fill = 0) %>% 
  mutate(
    ceup = CEU * 100/sum(CEU),
    eastp = East * 100 / sum(East),
    canonical = ifelse(title %in% canonical, 'canonical', 'others'),
  )

df3 <- df2 %>% 
  filter(world %in% c('East', 'Central EU')) %>% 
  mutate(world = ifelse(world == 'Central EU', 'CEU', world)) %>% 
  count(title, world) %>% 
  pivot_wider(id_cols = title,
              names_from = world,
              values_from = n,
              values_fill = 0) %>% 
  mutate(
    ceup = CEU * 100/sum(CEU),
    eastp = East * 100 / sum(East),
    canonical = ifelse(title %in% canonical, 'canonical', 'others'),
  )

df3 %>% 
  ggplot(aes(x = ceup, y = eastp, color = canonical)) +
  geom_jitter() +
  geom_text_repel(aes(label = title)) +
  geom_abline(color = 'cornflowerblue') +
  theme_bw() +
  labs(
    x = 'Central EU socialist regions',
    y = 'other socialist regions',
    color = NULL,
    title = 'percentage of editions'
  )
ggsave(
  'images/jokai/socialist-canon-ce-vs-non-ce.png',
  width = 8, height = 6,
  units = 'in', dpi = 300)

lang_nr <- df2 %>% 
  filter(world %in% c('East', 'Central EU')) %>% 
  mutate(world = ifelse(world == 'Central EU', 'CEU', world)) %>% count(world, lang) %>% 
  count(world)

lang_nr
df4 <- df2 %>% 
  filter(world %in% c('East', 'Central EU')) %>% 
  mutate(world = ifelse(world == 'Central EU', 'CEU', world)) %>% 
  select(title, world, lang) %>% 
  distinct() %>% 
  group_by(title, world) %>% 
  summarise(
    n = n(),
    .groups = 'drop'
  ) %>% 
  left_join(lang_nr, by = join_by(world)) %>% 
  mutate(p = n.x * 100 / n.y) %>%
  select(-c(n.x, n.y)) %>% 
  pivot_wider(id_cols = title,
              names_from = world,
              values_from = p,
              values_fill = 0.0) %>% 
  mutate(
    canonical = ifelse(title %in% canonical, 'canonical', 'others')
  )

df4

df4 %>% 
  ggplot(aes(x = CEU, y = East, color = canonical)) +
  geom_text_repel(aes(label = title), size = 2) +
  geom_jitter(alpha = 0.5) +
  geom_abline(color = 'cornflowerblue') +
  theme_bw() +
  labs(
    x = 'Central EU socialist regions',
    y = 'other socialist regions',
    color = NULL,
    title = 'percentage of languages'
  ) +
  scale_x_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 110)) +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     limits = c(0, 110))
ggsave(
  'images/jokai/socialist-canon-ce-vs-non-ce-lang.png',
  width = 6, height = 5,
  units = 'in', dpi = 300)
