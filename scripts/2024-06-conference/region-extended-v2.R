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

segments <- tribble(
  ~name,                    ~positive, ~negative,
  'de-sovjetized Europe',       '1|2',   '4',
  'eastern block',              '2|4',   NA,
  'eastern block without west', '2|4',   '1',
  'Europe',                     '1|2',   NA,
  'all',                        NA,      NA
)
segments %>% 
  mutate(id = gsub(' ', '-', name)) %>% 
  select(name, id)

for (i in 1:nrow(segments)) {
  row <- segments[i,]
  au <- selectWorld(all_authors_df, row$positive, row$negative)
  print(paste0(row$name, ': ', length(au)))
}

for (i in 1:nrow(segments)) {
  row <- segments[i,]
  suffix <- gsub(" ", '-', row$name)
  print(row$name)
  print(suffix)
  ratios <- calculate_ratios2(all_authors_df, row$positive, row$negative, 
                              authors_by_region, base_stats)
  print(head(ratios))
  file_name <- paste0('data/ratios-by-regions-', suffix, '.csv')
  print(file_name)
  write_csv(ratios, file = file_name)
}

