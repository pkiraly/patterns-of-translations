library(tidyverse)

df <- read_csv('data/city-year-languages.csv')
df.geocoded <- read_csv('data/cities-geocoded.csv')
df.geocoded <- df.geocoded %>% filter(geoid != "null")

map.europe <- map_data("world")
countries <- c('Italy')
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

ratio <- (borders$maxx - borders$minx) / (borders$maxy - borders$miny)
ratio

names(df)
years = seq(1800, 1970, 10)

dir <- 'images/gif'
if (!dir.exists(dir)) {
  dir.create(dir)
}
for (i in years) {
  j <- i + 9
  title <- paste0(i, '-', j)
  print(title)
  
  decade <- df %>% 
    filter(year_n >= i & year_n <= j & !is.na(city)) %>% 
    select(city) %>% 
    group_by(city) %>% 
    count() %>% 
    left_join(df.geocoded, by="city") %>% 
    mutate(
      lat = as.double(lat),
      long = as.double(long)
    )

  ggplot() +
    geom_polygon(
      data = map.europe,
      aes(x = long, y = lat, group = group, fill = region)
    ) +
    geom_point(
      data = decade,
      aes(x = long, y = lat, size = n),
      color = "red",
      alpha = .8) +
    geom_text(
      data = decade,
      mapping = aes(x = long, y = lat, label = name),
      nudge_y = -0.05,
      size = 1.8
    ) +
    coord_cartesian(
      xlim = c(borders$minx, borders$maxx),
      ylim = c(borders$miny, borders$maxy),
    ) +
    ggtitle(
      title,
      subtitle = 'Hungarian literature in foreign languages'
    ) +
    theme(
      legend.position = 'none',
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
    )

  ggsave(paste0(dir, '/italy-', title, '.png'), 
         width = 3.5, height = 4, units = 'in', dpi = 300)
  # Carpathian
  # width = 4.5, height = 3, units = 'in', dpi = 300)
  
}

