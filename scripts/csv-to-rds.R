# converts merged-Demeter-and-IT-(books-only)-v4.csv to RDS format
# it checks the types
library(tidyverse)

# csv_file <- 'data_raw/merged-Demeter-and-IT-(books-only)-v4-2026-02-20.tsv'
csv_file <- 'data_raw/merged-Demeter-and-IT-(books-only)-v4-2026-06-16.tsv'
col_names <- c(
#  'flagged',
  'id',
  'author',
  'orig_title',
  'genre',
  'orig_pub_yr',
  'orig_publ_city',
  'targ_lan_n',
  'country',
  'world',
  'target_title',
  'translator',
  'megjelenes',
  'megjegyzes',
  'city_n',
  'year_n',
  'editionstat',
  'isPartOf',
  'HU-minor',
  'interm_title',
  'interm_lang',
  'is_container',
  'series',
  'db',
  'auth_quality',
  'transl_quality',
  'publisher',
  'pagination',
  'orig_lang',
  'isbn',
  'kotet',
  'city',
  'year',
  'targ_lan'
)
col_types <- cols(
#  flagged = col_logical(),
  `id` = col_double(),
  `author` = col_character(),
  `orig_title` = col_character(),
  `genre` = col_character(),
  `orig_pub_yr` = col_double(),
  `orig_publ_city` = col_character(),
  `targ_lan_n` = col_character(),
  `country` = col_character(),
  `world` = col_double(),
  `target_title` = col_character(),
  `translator` = col_character(),
  `megjelenes` = col_character(),
  `megjegyzes` = col_character(),
  `city_n` = col_character(),
  `year_n` = col_double(),
  `isPartOf` = col_double(),
  `editionstat` = col_character(),
  `HU-minor` = col_character(),
  `interm_title` = col_character(),
  `interm_lang` = col_character(),
  `is_container` = col_logical(),
  `series` = col_character(),
  `db` = col_character(),
  `auth_quality` = col_logical(),
  `transl_quality` = col_logical(),
  `publisher` = col_character(),
  `pagination` = col_character(),
  `orig_lang` = col_character(),
  `isbn` = col_character(),
  `kotet` = col_character(),
  `city` = col_character(),
  `year` = col_character(),
  `targ_lan` = col_character()
)
df <- read_tsv(csv_file, 
               skip = 1,
               col_names = col_names, 
               col_types = col_types)

problems_df <- problems()
if (nrow(problems_df) > 0) {
  print(problems_df)
}

# skip this, we does not have flagged now
# df2 <- df %>% 
#   filter(!flagged) %>% 
#   select(-c(flagged))

saveRDS(df, 'data_raw/merged-Demeter-and-IT-(books-only)-v4-2026-06-16.rds')

