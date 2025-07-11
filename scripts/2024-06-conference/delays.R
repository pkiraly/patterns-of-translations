#' Display delays of translations 1947-1989
#'

library(tidyverse)

delay_breaks <- c(-1, 4, 10, 20, 40)
year_segment <- 5

df <- readRDS('data_raw/postwar.rds')
calculated_pub_years <- read_csv('data_raw/calculated_pub_years.csv')
calculated_pub_years

world2 <- df %>% 
  filter(country != "Hungary") %>% 
  filter(world != 3) %>% 
  left_join(calculated_pub_years, by = join_by(id)) %>% 
  mutate(
    orig_pub_yr = ifelse(
      is.na(orig_pub_yr) & !is.na(calculated_pub_yr),
      calculated_pub_yr,
      orig_pub_yr
    )
  ) %>% 
  filter(!is.na(orig_pub_yr))
nrow(world2)

min_year <- min(world2$year_n)
max_year <- max(world2$year_n)

prepared <- world2 %>% 
  mutate(
    delay = year_n - orig_pub_yr,
    year_range = min_year + (floor((year_n-min_year) / year_segment) * year_segment),
    year_range = sprintf(
      '%d-%d',
      year_range, 
      ifelse(max_year >= (year_range + year_segment - 1), 
             year_range + year_segment - 1,
             max_year))
  ) %>% 
  select(delay, world, year_range) %>% 
  group_by(year_range, world, delay) %>% 
  count()

total_books_per_segments <- prepared %>%
  ungroup() %>% 
  # filter(!is.na(delay)) %>% 
  select(-delay) %>% 
  group_by(year_range, world) %>% 
  summarise(total_books = sum(n), .groups = 'drop')

dgmin <- min(prepared$delay, na.rm = T) - 1
dgmax <- max(prepared$delay, na.rm = T)
delay_breaks <- c(dgmin, delay_breaks, dgmax)

groups <- prepared %>% 
  ungroup() %>% 
  mutate(
    delay1 = cut(delay, breaks = delay_breaks) 
  )

labels <- data.frame(b = delay_breaks) %>% 
  mutate(
    c = ifelse(b+1 == lead(b),
               as.character(b+1),
               sprintf('%s-%s', b+1,  lead(b))),
    d = lead(b)
  ) %>% 
  filter(!is.na(d)) %>% 
  select(c) %>% 
  unlist(use.names = FALSE)

delays <- groups %>% 
  select(delay1) %>% 
  count(delay1) %>% 
  mutate(delay_label = labels) %>% 
  select(-n)

g2 <- groups %>% 
  left_join(delays) %>% 
  mutate(delay2 = factor(delay_label, levels = labels)) %>% 
  select(-c(delay1, delay_label))

g3 <- g2 %>% 
  left_join(total_books_per_segments) %>% 
  mutate(percent = n * 100 / total_books) %>% 
  group_by(year_range, world, delay2) %>% 
  summarise(
    percent = sum(percent),
    .groups = "drop"
  ) %>% 
  group_by(year_range, world) %>% 
  reframe(
    delay2 = delay2,
    percent = percent,
    p2 = cumsum(percent),
    color = ifelse(p2 < 50, 'under 50%', 'above 50%')
  ) %>% 
  mutate(world = factor(world, levels = c(1, 2, 3)))

g3 %>% 
  ggplot(aes(x = delay2, y = percent)) +
  geom_col(aes(fill = color)) +
  geom_line(aes(x = delay2, y = p2, group = 1, color = color)) +
  facet_grid(year_range ~ world) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 45))

#'
#' bars: periods, facets: worlds
#'
g3 %>% 
  ggplot(aes(x = fct_rev(world), y = percent, fill = fct_rev(delay2))) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(vars(year_range),
             nrow = 9,
             dir = "v",
             strip.position = "right",
  ) +
  theme_bw() +
  theme(
    strip.text.y = element_text(angle = 0),
    strip.background = element_rect(fill = 'white')) +
  coord_flip(clip = "off") +
  labs(
    title = 'Delay from original publication',
    x = 'world system'
  ) +
  scale_fill_discrete(name = "delay in years") +
  scale_x_discrete(
    # breaks = c(3, 2, 1),
    breaks = c(1, 2, 3),
    labels = c('1' = '1st', '2' = '2nd', '3' = '3rd')
  )

world_label <- function(w) {
  print(w)
  labels <- c('1st world',
    '2nd world',
    '3rd world')
  labels[as.numeric(w)]
}

#'
#' bars: worlds, facets: periods
#' position: horizontal
#'
g3 %>% 
  ggplot(aes(x = year_range, y = percent, fill = fct_rev(delay2))) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(vars(world), ncol = 3, 
             labeller = labeller(world = world_label)) +
  theme_bw() +
  labs(
    title = 'Delay from original publication',
    x = 'period'
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_discrete(name = "delay in years")
