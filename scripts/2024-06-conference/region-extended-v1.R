library(tidyverse)
library(gdata)
source('scripts/2024-06-conference/region_functions.R')

world2 <- readRDS('data_raw/postwar-with-regions.rds')

authors_by_region <- world2 %>% select(author, region) %>%
  distinct()
authors_by_region

nr_authors_by_region <- world2 %>% select(author, region) %>%
  distinct() %>% 
  group_by(region) %>% 
  summarize(authors = n())
print(nr_authors_by_region, n = Inf)

authors_by_region2 <- world2 %>% select(author, region, year_n) %>%
  distinct() %>% 
  group_by(author, region) %>% 
  summarize(first_year = min(year_n), .groups = "drop")
authors_by_region2

nr_books_by_region <- world2 %>% select(region) %>% 
  group_by(region) %>% 
  summarize(books = n()) %>% 
  arrange(desc(books))
print(nr_books_by_region, n = Inf)

base_stats <- nr_authors_by_region %>% 
  full_join(nr_books_by_region, join_by(region))
base_stats

all_authors_df <- world2 %>% 
  select(author, world) %>% 
  arrange(author, world) %>% 
  distinct() %>% 
  group_by(author) %>% 
  summarise(
    world = paste(world, collapse = ", ")
  )

all_authors_df

all_authors_df %>% 
  count(world) %>% 
  mutate(world = gsub(', ', '&', world))

worlds <- all_authors_df %>% 
  count(world) %>% 
  select(world) %>% unlist(use.names = FALSE)
worlds

data.frame(label = worlds) %>% 
  mutate(id = gsub(", ", '-', label))

worlds <- c(worlds, 'all')
source('scripts/2024-06-conference/region_functions.R')

for (current_world in worlds) {
  suffix <- gsub(", ", '-', current_world)
  print(current_world)
  print(suffix)
  ratios <- calculate_ratios(current_world, all_authors_df, authors_by_region, base_stats)
  print(head(ratios))
  file_name <- paste0('data/ratios-by-regions-', suffix, '.csv')
  print(file_name)
  write_csv(ratios, file = file_name)
}


selected_authors_by_region <- authors_by_region
l <- length(base_stats$region)
ratios <- as_tibble(data.frame(c1 = character(),
                               c2 = character(),
                               ratio = numeric(),
                               common_authors = numeric(),
                               first = numeric()))
country1 <- 'USSR-grúz'
country2 <- 'USSR-orosz'
source('scripts/2024-06-conference/region_functions.R')
ratios <- calculate_pair(ratios, 
                         country1, country2, 
                         selected_authors_by_region,
                         base_stats)
ratios

#'------------------
authors <- c("PETŐFI Sándor", "HIDAS Antal", "KARINTHY Ferenc", "MOLNÁR Géza",
             "NAGY Sándor", "HELTAI Jenő", "BERKESI András", "MADÁCH Imre",
             "ADY Endre", "FÖLDEÁK János", "MÓRA Ferenc", "SARKADI Imre",
             "MIKSZÁTH Kálmán", "MOLNÁR Ferenc", "DÉRY Tibor", "ÖRKÉNY István",
             "FEHÉR Klára", "GÁBOR Andor", "JÓKAI Mór", "KARINTHY Frigyes")
regions <- c('USSR-grúz', 'USSR-orosz')
# get_score_for_first_publication(authors, countries)

base_scores <- data.frame(region = regions, score = c(0, 0))

calulated_scores <- authors_by_region2 %>% 
  filter(author %in% authors & region %in% regions) %>% 
  group_by(author) %>%
  mutate(y = min(first_year)) %>% 
  filter(y == first_year) %>% 
  group_by(author) %>% 
  mutate(score = 1 / n()) %>% 
  group_by(region) %>% 
  summarise(score = sum(score), .groups = "drop")

base_scores %>% 
  left_join(calulated_scores, join_by(region)) %>% 
  mutate(score = ifelse(!is.na(score.y), score.y, score.x)) %>% 
  select(region, score)
#'------------------


ratios <- calculate_ratios('all', all_authors_df, authors_by_region, base_stats)
ratios
ratios %>% 
  filter((region1 == 'USSR-grúz' & region2 == 'USSR-orosz') |
           (region2 == 'USSR-grúz' & region1 == 'USSR-orosz')
  ) %>% 
  print(n = Inf)
print(head(ratios))
