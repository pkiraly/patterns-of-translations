library(rvest)
library(tidyverse)
library(ggmap)
library(stringr)

df <- read_csv('data/city-by-works.csv')
df.geocoded <- read_csv('data/cities-geocoded.csv')
df.geocoded <- df.geocoded %>% filter(geoid != "null")

df.geocoded %>% 
  group_by(geoid) %>% 
  count() %>% 
  filter(n > 1)

df.euro_cities <- df %>% 
  left_join(df.geocoded, by="city") %>% 
  mutate(
    lat = as.double(lat),
    long = as.double(long)
  )
df.euro_cities

map.europe <- map_data("world")

df.euro_cities

#%>% 
#  filter(long >= -9 & long <= 45 & lat >= 32 & lat <= 70)
view(map.europe)

countries <- c('Hungary', 'Slovakia', 'Romania')

borders <- map.europe %>%
  filter(region %in% countries) %>%  # & is.na(subregion)) %>% 
    mutate(
    minx = min(long),
    maxx = max(long),
    miny = min(lat),
    maxy = max(lat),
  ) %>% 
  select(minx, maxx, miny, maxy) %>% 
  head(1)

cities <- df.euro_cities
# cities <- df.euro_cities %>% filter(country %in% countries)

ggplot() +
  geom_polygon(
    data = map.europe,
    aes(x = long, y = lat, group = group, fill = region)
  ) +
  geom_point(
    data = cities,
    aes(x = long, y = lat, size = works),
    color = "red",
    alpha = .8) +
  geom_text(
    data = cities,
    mapping = aes(x = long, y = lat, label = name),
    nudge_y = -0.05,
    size = 3
  ) +
  # Europe
  # coord_cartesian(xlim = c(-21,50), ylim = c(32,70)) +
  # Balkan
  # coord_cartesian(xlim = c(12.5,29), ylim = c(40,50)) +
  # D/A/CH
  # coord_cartesian(xlim = c(6,17), ylim = c(45.7,54.5)) +
  # Benelux
  # coord_cartesian(xlim = c(2.7,7.2), ylim = c(49.5,53.5)) +
  coord_cartesian(
    xlim = c(borders$minx, borders$maxx),
    ylim = c(borders$miny, borders$maxy),
  ) +
  theme(
    legend.position = 'none',
    axis.title = element_blank(),
  )

