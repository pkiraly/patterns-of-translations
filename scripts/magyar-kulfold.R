library(tidyverse)
library(zoo)

get_all_in_abroad <- function(all_cities, all_years) {
  l <- length(all_cities)
  target <- rep(-1, l)
  for (i in 1:l) {
    splitted <- strsplit(all_cities[i], "−")[[1]]
    countries <- get_country(splitted)
    cat <- TRUE
    if ('Hungary' %in% countries) {
      cat <- FALSE
    } else if (TRUE %in% (divided_countries %in% countries)) {
      year <- all_years[i]
      if (year < 1920) {
        if ('Slovakia' %in% countries) {
          cat <- FALSE
        } else if (TRUE %in% (pre_1920 %in% splitted)) {
          cat <- FALSE
        }
      } else if (year >= 1938 && year < 1945) {
        if (TRUE %in% (war_times %in% splitted)) {
          cat <- FALSE
        }
      }
    }
    
    l[i] <- cat
  }
  return(l)
}

get_category <- function(all_cities) {
  l <- length(all_cities)
  target <- rep(-1, l)
  for (i in 1:l) {
    splitted <- strsplit(all_cities[i], "−")[[1]]
    l[i] <- ifelse(
      'Hungary' %in% get_country(splitted),
      FALSE, TRUE
    )
  }
  return(l)
}

get_country <- function(individdual_cities) {
  m <- cities %>%
    filter(city %in% individdual_cities) %>% 
    select(country) %>% 
    distinct(country) %>% 
    unlist(use.names = FALSE)
  return(m)
}

df <- read_csv('data/demeter-2021-09-13.csv')
cities <- read_csv('data/cities-geocoded.csv')

divided_countries <- c('Romania', 'Slovakia', 'Serbia', 'Ukraine', 'Slovenia', 'Croatia')

pre_1920 <- c(
  'Rijeka',
  # Romania
  'Arad', 'Baia Mare', 'Bistriţa', 'Braşov', 'Cluj-Napoca', 'Dej', 'Fogărăs',
  'Gherla', 'Oradea', 'Orăştie', 'Periam', 'Satu-Mare', 'Sibiu', 'Sighișoara',
  'Târgu-Mureş', 'Timișoara', 'Turda',
  # Serbia
  'Kikinda', 'Novi Sad', 'Pančevo', 'Subotica', 'Versec', 'Zrenjanin',
  # Slovenia
  'Murska Sobota',
  # Ukraine
  'Užgorod'
)

war_times <- c(
  # Romania
  'Baia Mare', 'Bistriţa', 'Cluj-Napoca', 'Dej', 'Gherla', 'Oradea', 'Satu-Mare',
  'Târgu-Mureş', 'Turda',
  # Serbia
  'Kikinda', 'Novi Sad', 'Pančevo', 'Subotica', 'Versec', 'Zrenjanin',
  # Ukraine
  'Užgorod',
  # Slovakia
  'Košice', 'Levoča', 'Lučenec'
)

city_year <- df %>% 
  filter(isPartOf %in% c(-1, -3)) %>% 
  select(normalized_city, year_n) %>% 
  filter(!is.na(normalized_city)) %>% 
  filter(!is.na(year_n))

city_distance <- city_year %>% 
  select(normalized_city) %>% 
  distinct(normalized_city) %>% 
  mutate(in_abroad = get_category(normalized_city))

# advanced
publication_abroad <- city_year %>% 
  mutate(in_abroad = get_all_in_abroad(normalized_city, year_n))

data_to_display <- publication_abroad %>% 
  group_by(year_n) %>% 
  summarise(
    avg_in_abroad = mean(in_abroad, na.rm = TRUE) * 100,
    count = n()
  ) %>% 
  mutate(rollavg = rollmean(avg_in_abroad, 5, align = 'center', fill = c(0, 0, 0))) %>% 
  mutate(rollavg = ifelse(year_n < 1978, rollavg, avg_in_abroad))

data_to_display %>% 
  ggplot(aes(x = year_n, y = rollavg)) +
  geom_point(aes(x = year_n, y = avg_in_abroad, size = count), alpha=0.2) +
  geom_line(aes(size=count), colour = 'darkblue') +
  labs(
    title='A külföldön megjelent könyvek aránya',
    caption = '5 éves mozgóátlag',
    size = 'éves\nkönyv-\nszám'
  ) +
  ylab('külföldi megjelenés aránya (%)') +
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

ggsave("images/publications-in-abroad-advanced.png", 
       width = 6, height = 4, units = 'in', dpi = 300)

counted <- publication_abroad %>% 
  group_by(year_n, in_abroad) %>% 
  summarise(
    .groups = "keep",
    count = n(),
  ) %>% 
  ungroup()
counted

data_to_display <- counted %>% 
  arrange(year_n) %>% 
  group_by(in_abroad) %>% 
  summarise(
    .groups = "keep",
    year_n = year_n,
    count = count,
    rollavg = rollmean(count, 5, align = 'center', fill = c(0, 0, 0)),
    rollavg = ifelse(year_n < 1978, rollavg, count)
  ) %>% 
  mutate(
    in_abroad = ifelse(in_abroad, "külföldi", "hazai"),
  )

data_to_display %>% 
  ggplot(aes(x = year_n, y = count)) +
  geom_point(aes(color = in_abroad, size=count), alpha=0.2) +
  geom_line(aes(y = rollavg, group = in_abroad, size = count), colour = 'darkblue', alpha=.2) +
  labs(
    title='A hazai és külföldi megjelenések száma (kötetek)',
    caption = '5 éves mozgóátlag',
    color = 'megjelenés',
    size = 'éves\nkönyv-\nszám'
  ) +
  ylab('kötetszám') +
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

ggsave("images/publications-in-abroad-absolute.png", 
       width = 6, height = 4, units = 'in', dpi = 300)

data_to_display %>% 
  filter(year_n >= 1945 & year_n <= 1970) %>% 
  ggplot(aes(x = year_n, y = count)) +
  geom_point(aes(color = in_abroad, size=count), alpha=0.2) +
  geom_line(aes(y = rollavg, group = in_abroad, size = count), colour = 'darkblue', alpha=.2) +
  labs(
    title='A hazai és külföldi megjelenések száma (kötetek, 1945-1970)',
    caption = '5 éves mozgóátlag',
    color = 'megjelenés',
    size = 'éves\nkönyv-\nszám'
  ) +
  ylab('kötetszám') +
  xlab('megjelenési év') +
  scale_x_continuous(
    breaks = seq(1945, 1970, by=1),
    labels = seq(1945, 1970, by=1),
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    plot.caption = element_text(color = 'darkblue')
  )

ggsave("images/publications-in-abroad-absolute-1945-1970.png", 
       width = 6, height = 4, units = 'in', dpi = 300)

publication_abroad %>% 
  filter(year_n %in% c(1956, 1957, 1958, 1959, 1960)) %>% 
  filter(in_abroad == 1) %>% 
  count(normalized_city, year_n) %>% 
  arrange(desc(n)) %>% 
  spread(key = year_n, value =  n, fill = 0) %>% 
  mutate(total = `1956` + `1957` + `1958` + `1959` + `1960`) %>% 
  arrange(desc(total)) %>% 
  view()
  
df %>% 
  filter(isPartOf %in% c(-1, -3)) %>% 
  filter(year_n == 1958) %>% 
  filter(normalized_city == 'Moscow') %>% 
  select(szerzo) %>% 
  distinct() %>% 
  arrange(szerzo)

df %>% 
  filter(magyar_cim == 'János vitéz') %>% 
  select(idegen_cim) %>% 
  count(idegen_cim) %>% 
  arrange(desc(n)) %>% 
  view()
  
# naive approach
# city_year %>% 
#   left_join(city_distance, by = ('normalized_city' = 'normalized_city')) %>% 
#   group_by(year_n) %>% 
#   summarise(
#     avg_in_abroad = mean(in_abroad, na.rm = TRUE) * 100,
#     count = n()
#   ) %>% 
#   mutate(rollavg = rollmean(avg_in_abroad, 5, align = 'center', fill = c(0, 0, 0))) %>%
#   ggplot(aes(x = year_n, y = rollavg)) +
#   geom_point(aes(x = year_n, y = avg_in_abroad, size = count), alpha=0.2) +
#   geom_line(colour = 'darkblue') +
#   labs(
#     title='A külföldön megjelent könyvek aránya',
#     caption = '5 éves mozgóátlag',
#     size = 'éves\nkönyv-\nszám'
#   ) +
#   ylab('külföldi megjelenés aránya (%)') +
#   xlab('megjelenési év') +
#   scale_x_continuous(
#     breaks = seq(1790, 1980, by=10),
#     labels = seq(1790, 1980, by=10),
#   ) +
#   theme_bw() +
#   theme(
#     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
#     plot.caption = element_text(color = 'darkblue')
#   )
# 
# ggsave("images/publications-in-abroad-naive.png", 
#        width = 6, height = 4, units = 'in', dpi = 300)
