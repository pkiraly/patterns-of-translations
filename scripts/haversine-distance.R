library(pracma)
library(tidyverse)
library(hablar)
#library(RcppRoll)
library(zoo)
source('scripts/common-functions.R')

get_all_avg_distances <- function(all_cities) {
  l <- length(all_cities)
  target <- rep(-1, l)
  for (i in 1:l) {
    splitted <- strsplit(all_cities[i], "−")[[1]]
    l[i] <- get_avg_distance(splitted)
  }
  return(l)
}

get_avg_distance <- function(individdual_cities) {
  m <- cities_with_distance %>%
    filter(city %in% individdual_cities) %>% 
    select(dist) %>% 
    summarise(m = mean(dist, na.rm = TRUE)) %>% 
    unlist(use.names = FALSE)
  return(m)
}

distance_haversine2 <- function(lat, lon) {
  l <- length(lat)
  target <- rep(-1, l)
  for (i in 1:l) {
    l[i] <- distance_haversine(BUD, c(lat[i], lon[i]))
  }
  return(l)
}

distance_haversine <- function(point1, point2) {
  earth_radius  <-  6371 # in km
  lat1 <- point1[1]
  lon1 <- point1[2]
  lat2 <- point2[1]
  lon2 <- point2[2]
  delta_lat <- lat2 - lat1
  delta_lon <- lon2 - lon1
  alpha <- delta_lat / 2
  beta  <- delta_lon / 2
  a <- sin(deg2rad(alpha)) * sin(deg2rad(alpha)) + cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * sin(deg2rad(beta)) * sin(deg2rad(beta))
  c <- asin(min(1, sqrt(a)))
  distance <- 2 * earth_radius * c
  distance <- round(distance, 4)
  return(distance)
}

df <- read_csv('data/demeter-2021-09-13.csv')
cities <- read_csv('data/cities-geocoded.csv')
cities
BUD <-cities %>% filter(city == 'Budapest') %>% 
  select(lat, long) %>% convert(dbl(lat, long)) %>% 
  unlist(use.names = FALSE)

cities_with_distance <- cities %>%
  filter(!is.na(lat) & lat != "null") %>% 
  filter(!is.na(long) & long != "null") %>% 
  convert(dbl(lat, long)) %>% 
  mutate(dist = distance_haversine2(lat, long))

# second approach
city_year <- df %>% 
  filter(isPartOf %in% c(-1, -3)) %>% 
  select(normalized_city, year_n) %>% 
  filter(!is.na(normalized_city)) %>% 
  filter(!is.na(year_n))

city_distance <- city_year %>% 
  select(normalized_city) %>% 
  distinct(normalized_city) %>% 
  mutate(dist = get_all_avg_distances(normalized_city)) %>% 
  filter(!is.na(dist)) %>% 
  view()

data_to_display <- city_year %>% 
  left_join(city_distance, by = ('normalized_city' = 'normalized_city')) %>% 
  group_by(year_n) %>% 
  summarise(
    avg_dist = mean(dist, na.rm = TRUE),
    count = n()
  ) %>% 
  select(year_n, avg_dist, count) %>% 
  # mutate(avg_dist2 = roll_mean(avg_dist, n=5, fill=c(), na.rm = TRUE)) %>%
  mutate(avg_dist2 = rollmean(avg_dist, 5, align = 'center', fill = c(0, 0, 0))) %>% 
  mutate(avg_dist2 = ifelse(year_n < 1978, avg_dist2, avg_dist))

data_to_display %>% 
  ggplot(aes(x = year_n, y = avg_dist2)) +
  geom_line(aes(size = count), colour = 'darkblue', alpha=0.5) +
  geom_point(aes(x = year_n, y = avg_dist, size=count), alpha=0.2) +
  labs(
    title='A megjelenési helyek átlagos légvonalbeli távolsága Budapesttől',
    caption = '5 éves mozgóátlag',
    size = 'éves\nkönyv-\nszám'
  ) +
  ylab('átlagos távolság (km)') +
  xlab('megjelenési év') +
  scale_x_continuous(
    breaks = seq(1790, 1980, by=10),
    labels = seq(1790, 1980, by=10),
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    plot.caption = element_text(color = 'darkblue')
  )

ggsave("images/distance-of-publication-place-v2.png", 
       width = 6, height = 4, units = 'in', dpi = 300)

# first approach
# df %>% 
#   filter(isPartOf %in% c(-1, -3)) %>% 
#   select(normalized_city, year_n) %>% 
#   filter(!is.na(normalized_city)) %>% 
#   filter(!is.na(year_n)) %>% 
#   mutate(normalized_city2 = str_replace(normalized_city, "−.*$", "")) %>% 
#   left_join(cities_with_distance, by = c("normalized_city2" = "city")) %>% 
#   group_by(year_n) %>% 
#   summarise(avg_dist = mean(dist, na.rm = TRUE)) %>% 
#   select(year_n, avg_dist) %>% 
#   # mutate(avg_dist2 = roll_mean(avg_dist, n=5, fill=c(), na.rm = TRUE)) %>%
#   mutate(avg_dist2 = rollmean(avg_dist, 5, align = 'center', fill = c(0, 0, 0))) %>%
#   ggplot(aes(x = year_n, y = avg_dist2)) +
#   geom_line(colour = 'red') +
#   geom_point(aes(x = year_n, y = avg_dist), alpha=0.5) +
#   ggtitle('A megjelenési helyek átlagos távolsága Budapesttől',
#           subtitle = 'vörös vonal: 5 éves futó átlag') +
#   ylab('átlagos távolság (km)') +
#   xlab('megjelenési év')
# 
# ggsave("images/distance-of-publication-place.png", 
#        width = 6, height = 4, units = 'in', dpi = 300)
