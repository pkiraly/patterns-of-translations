#' Display delays of translations 1947-1989
#'

library(tidyverse)

delay_breaks <- c(-1, 4, 10, 20, 40)
year_segment <- 5

df <- readRDS('data_raw/postwar.rds')
calculated_pub_years <- read_csv('data_raw/calculated_pub_years.csv')
calculated_pub_years

countries <- c('Czechoslovakia', 'GDR', 'Poland', 'USSR')

world2 <- df %>% 
  filter(country != "Hungary") %>% 
  left_join(calculated_pub_years, by = join_by(id)) %>% 
  mutate(
    orig_pub_yr = ifelse(
      is.na(orig_pub_yr) & !is.na(calculated_pub_yr),
      calculated_pub_yr,
      orig_pub_yr
    )
  ) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
#  filter(world == 2) %>% 
  filter((world == 1) | (country %in% countries))
nrow(world2)
world2

min_year <- min(world2$year_n)
max_year <- max(world2$year_n)

ce <- c('Czechoslovakia', 'GDR', 'Poland')
prepared <- world2 %>% 
  mutate(
    delay = year_n - orig_pub_yr,
    year_range = min_year + (floor((year_n-min_year) / year_segment) * year_segment),
    year_range = sprintf(
      '%d-%d',
      year_range, 
      ifelse(max_year >= (year_range + year_segment - 1), 
             year_range + year_segment - 1,
             max_year)),
    region = ifelse(
      country %in% ce, 
      'Central Europe', 
      ifelse(
        country == 'USSR',
        country,
        '1st world'
      )
    ),
    region = factor(region, levels = c('1st world', 'Central Europe', 'USSR'))
  ) %>% 
  select(delay, region, year_range) %>% 
  group_by(year_range, region, delay) %>% 
  count()

total_books_per_segments <- prepared %>%
  ungroup() %>% 
  # filter(!is.na(delay)) %>% 
  select(-delay) %>% 
  group_by(year_range, region) %>% 
  summarise(total_books = sum(n), .groups = 'drop')

total_books_per_segments

dgmin <- min(prepared$delay, na.rm = T) - 1
dgmax <- max(prepared$delay, na.rm = T)
delay_breaks <- c(dgmin, delay_breaks, dgmax)
delay_breaks

print(prepared, n = Inf)
prepared %>% 
  ungroup() %>% 
  select(region) %>% 
  distinct()

groups <- prepared %>% 
  ungroup() %>% 
  mutate(
    delay1 = cut(delay, breaks = delay_breaks) 
  )
print(groups, n = Inf)

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

print(groups, n = Inf)
total_books_per_segments

g2 <- groups %>% 
  left_join(delays) %>% 
  mutate(delay2 = factor(delay_label, levels = labels)) %>% 
  select(-c(delay1, delay_label))
g2
print(g2, n = Inf)

g2 %>% 
  left_join(total_books_per_segments) %>% 
  mutate(percent = n * 100 / total_books) %>% 
  group_by(year_range, region, delay2) %>% 
  summarise(
    percent = sum(percent),
    .groups = "drop"
  )%>% 
  group_by(year_range, region) %>% 
  reframe(
    delay2 = delay2,
    percent = percent,
    p2 = cumsum(percent),
    color = ifelse(p2 < 50, 'under 50%', 'above 50%')
  )

g3 <- g2 %>% 
  left_join(total_books_per_segments) %>% 
  mutate(percent = n * 100 / total_books) %>% 
  group_by(year_range, region, delay2) %>% 
  summarise(
    percent = sum(percent),
    .groups = "drop"
  ) %>% 
  group_by(year_range, region) %>% 
  reframe(
    delay2 = delay2,
    percent = percent,
    p2 = cumsum(percent),
    color = ifelse(p2 < 50, 'under 50%', 'above 50%')
  )
print(g3, n = Inf)

g3 %>% 
  ggplot(aes(x = delay2, y = percent)) +
  geom_col(aes(fill = color)) +
  geom_line(aes(x = delay2, y = p2, group = 1, color = color)) +
  facet_grid(year_range ~ region) +
  theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 45))

#'
#' bars: periods, facets: worlds
#'
g3 %>% 
  ggplot(aes(x = fct_rev(region), y = percent, fill = fct_rev(delay2))) +
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
    x = 'region'
  ) +
  scale_fill_discrete(name = "delay in years")

#'
#' bars: worlds, facets: periods
#' position: horizontal
#'
g3 %>% 
  ggplot(aes(x = year_range, y = percent, fill = fct_rev(delay2))) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(vars(region), ncol = 3) +
  theme_bw() +
  labs(
    title = 'Delay from original publication',
    x = 'period'
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_discrete(name = "delay in years")
