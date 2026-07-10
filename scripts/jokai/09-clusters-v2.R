library(tidyverse)
library(skmeans)
require("cluster")
library(ggpubr)
source('scripts/jokai/common-functions.R')

#' the minimum number languages a work is translated to
min_languages <- 3
#' the number of clusters
cluster_count <- 3
#' calulate with ratio or calculate with frequency
calculate_with_ratio <- TRUE
#' deadline
deadline <- 1920
deadline <- FALSE
without_german <- TRUE

df <- readRDS('data_raw/jokai.rds')
df %>% count(genre)
# novella, novellák, versek, NA

years <- c(1800, 1920, 1949, 1989, 2025)
labels <- c()
for (i in 1:(length(years)-1)) {
  labels <- c(labels, sprintf("%d-%d", years[i]+1, years[i+1]))
}
labels



df2 <- df %>% 
  filter(!is.na(genre)) %>% 
  filter(!(genre %in% c('novella', 'novellák', 'versek'))) %>% 
  select(orig_title, lang = targ_lan_n, start = year_n, country, orig_pub_yr) %>% 
  filter(!is.na(lang) & !is.na(start) & !is.na(orig_title)) %>% 
  filter(!(orig_title %in% c('Források:'))) %>% 
  filter(!grepl(' − ', orig_title)) %>% 
  mutate(date_range = cut(start, breaks = years, labels = labels)) %>% 
  rename(title = orig_title) %>%
  mutate(
    title = ifelse(title == 'Óceánia, vagy egy elsüllyedt világrész története',
                   'Óceánia...', title),
    title = ifelse(title == 'És mégis mozog a föld: Eppur si muove',
                   'És mégis mozog a föld', title),
    title = ifelse(title == 'Forradalmi és csataképek 1848. és 1849-ből',
                   'Forradalmi és csataképek...', title),
  ) %>% 
  mutate(
    lang = case_match(
      lang,
      'német' ~ 'German',
      'angol' ~ 'English',
      'cseh' ~ 'Czech',
      'észt' ~ 'Estonian',
      'finn' ~ 'Finnish',
      'francia' ~ 'French',
      'lengyel' ~ 'Polish',
      'olasz' ~ 'Italian',
      'svéd' ~ 'Swedish',
      'szerb' ~ 'Serbian',
      'szlovák' ~ 'Slovakian',
      'bolgár' ~ 'Bulgarian',
      'latin' ~ 'Latin',
      'dán' ~ 'Danish',
      'orosz' ~ 'Russian',
      'horvát' ~ 'Croatian',
      'spanyol' ~ 'Spanish',
      'román' ~ 'Romanian',
      'örmény' ~ 'Armenian',
      'török' ~ 'Turkish',
      'holland' ~ 'Dutch',
      'kínai' ~ 'Chinese',
      'ukrán' ~ 'Ukranian',
      'szlovén' ~ 'Slovenian',
      'héber' ~ 'Hebrew',
      'grúz' ~ 'Georgian',
      'eszperantó' ~ 'Esperanto',
      'görög' ~ 'Greek',
      'lett' ~ 'Latvian',
      'rutén' ~ 'Ruthenian',
      'litván' ~ 'Lithuanian',
      'azerbajdzsán' ~ 'Azerbaijani',
      'tadzsik' ~ 'Tajik',
      'türkmén' ~ 'Turkmen',
      'katalán' ~ 'Catalan',
      'beás romani' ~ 'Boyash romani',
      'vietnámi' ~ 'Vietnamese',
    )
  ) %>% 
  mutate(
    lang = ifelse(between(start, 1945, 1989)
                  & lang == 'German' 
                  & !is.na(country) & country == 'GDR', 
                  paste0('DDR-', lang), lang),
    lang = ifelse(lang == 'German' 
                  & !is.na(country) 
                  & country == 'Hungary',
                  paste0('HU-', lang), lang),
    lang = ifelse(
      lang == 'English',
      ifelse(!is.na(country) & country == 'UK', paste0('UK-', lang),
             ifelse(!is.na(country) & country == 'USA', paste0('US-', lang),
                    ifelse(!is.na(country) & country == 'Hungary', paste0('HU-', lang),
                           lang))),
      lang
    ),
    lang = ifelse(between(start, 1945, 1989) & lang %in% c('Polish', 'Czech', 'Slovakian', 'Bulgarian', 'Slovenian', 'Lithuanian', 'Serbian', 'Croatian', 'Estonian', 'Romanian', 'Armenian', 'Ukranian', 'Georgian', 'Latvian', 'Ruthenian', 'Azerbaijani', 'Tajik', 'Turkmen', 'Vietnamese'), paste0(lang, '*'), lang),
    lang = ifelse(between(start, 1917, 1991) & lang %in% c('Russian'), paste0(lang, '*'), lang),
  )

orig_year <- df2 %>% select(title, year=orig_pub_yr) %>% 
  filter(!is.na(year)) %>%
  group_by(title) %>% 
  summarise(
    year = min(year)
  ) %>% 
  distinct()

date_ranges <- df2 %>% count(date_range) %>% 
  pull(date_range) %>% as.character()

for (.date_range in date_ranges) {
  print(.date_range)
  csv <- sprintf('data_raw/jokai/jokai-langs-%s.csv', .date_range)
  df2 %>% filter(date_range == .date_range) %>% 
    rename(language = lang) %>% 
    select(-date_range) %>% 
    write_csv(csv)
}

if (deadline != FALSE) {
  df2 <- df2 %>% 
    filter(start <= deadline)
}

df2 <- df2 %>% 
  select(-c(start, country))

selected_titles <- df2 %>%
  select(title, lang) %>% 
  distinct() %>% 
  group_by(title) %>% 
  summarise(c = n()) %>% 
  filter(c > min_languages) %>% 
  select(title) %>% pull()

selected_titles
#---------

create_clusters <- function(.df) {
  print('[df_langs]')
  df_langs <- .df %>% 
    count(title, lang) %>% 
    group_by(title) %>% 
    reframe(
      total = sum(n),
      n = n,
      lang = lang,
      value = n / total,
    ) %>% 
    select(-c(total, n)) %>% 
    pivot_wider(id_cols = c(title),
                names_from = lang, values_from = value,
                values_fill = 0)
  print(paste('df_langs dim():', df_langs %>% dim()))
  
  print('[langs_matrix]')
  langs_matrix = as.matrix(df_langs %>% select(-title))
  langs_matrix
  
  print('[skmeans]')
  hparty <- skmeans(langs_matrix, cluster_count,
                    control = list(verbose = FALSE))
  
  print('[lang_clusters]')
  cluster_ids <- dimnames(hparty$prototypes)[[1]]
  lang_clusters <- as_tibble(hparty$prototypes)
  lang_clusters$id <- cluster_ids
  print(lang_clusters)
  
  print('[clustered_titles]')
  print(paste('df_langs$title:', length(df_langs$title)))
  print(paste('hparty$cluster:', length(hparty$cluster)))
  clusters <- tibble(
    title = df_langs$title,
    cluster = hparty$cluster,
    author = names(hparty$cluster)
  )
  clustered_titles <- clusters %>% 
    group_by(cluster) %>% 
    mutate(
      y = row_number(),
      maxy = max(y),
      y2 = maxy - (y - 1), 
    )
  print(clustered_titles)
  return(list(clusters = lang_clusters,
              titles = clustered_titles))
}

draw_p2 <- function(.clusters) {
  limit <- 0.1
  p2 <- .clusters %>% 
    pivot_longer(1:(ncol(.clusters)-1)) %>% 
    mutate(
      id = factor(id),
      name1 = factor(name),
      color = ifelse(value >= limit, 'maroon', 'cornflowerblue'),
    ) %>% 
    ggplot(aes(x = name1, y = id)) +
    geom_point(aes(color = color, size = value),
               show.legend = FALSE) +
    geom_text(
      aes(label = ifelse(value >= limit,
                         sprintf("%s (%.2f)", name, value),
                         ''), 
          #x = as.numeric(name1) + 0.5,
          y = as.numeric(id) + 0.1,
      ),
      angle = 60, hjust = 0, vjust = 0,
      size = 2) +
    labs(
      y = 'cluster',
      x = 'languages',
      size = '',
    ) +
    scale_color_identity() +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 60, vjust = 1,
                                 hjust=1, size=6)) +
    scale_size_area()

  return(p2)
}

draw_titles <- function(.titles) {
  # print(head(.titles))
  clusters <- .titles %>% 
    left_join(orig_year) %>% 
    group_by(cluster) %>% 
    reframe(
      mean = round(mean(year, na.rm=TRUE)),
      sd = round(sd(year, na.rm=TRUE)),
      df = (max(year, na.rm = T) - min(year, na.rm = T)) / 2,
      df_label = ifelse(df == 0, '', sprintf('+-%sy.', df)),
      sd_label = ifelse(is.na(sd), '', sprintf( '+-%sy.', sd)),
      cluster_label = sprintf('%s (%s%s)', cluster, mean, df_label),
    ) %>% 
    select(cluster, cluster_label) %>% 
    distinct() %>% 
    print()
  
  # orig_year
  p1 <- .titles %>% 
    left_join(orig_year) %>% 
    left_join(clusters) %>% 
    mutate(
      y2 = max(.titles$y) - (y - 1),
      label = sprintf('%s (%s)', title, year)
    ) %>% 
    ggplot(aes(x = 1, y = y2)) + 
    geom_text(aes(label=label), size = 3) +
    facet_wrap(vars(cluster_label)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white"))
  return(p1)
}

draw_image <- function(.p1, .p2, .date_range) {
  pcommon <- ggarrange(.p1, .p2, ncol = 2, nrow = 1)
  annotate_figure(
    pcommon, 
    top = text_grob(.date_range, face = "bold", size = 14))
  ggsave(
    sprintf(
      'images/jokai/clusters2-by-language-%d-%s-%s.png',
      cluster_count,
      .date_range,
      ifelse(without_german == TRUE, 'without-german', 'with-german')
    ),
    width = 12, height = calculateHeight(12),
    units = 'in', dpi = 300)
}

draw_clusters <- function(.clusters, .date_range) {
  p1 <- draw_titles(.clusters$titles)
  p2 <- draw_p2(.clusters$clusters)
  draw_image(p1, p2, .date_range)
}

add_df <- function(.df1, .df2) {
  if (is.null(.df1)) {
    .df1 <- .df2
  } else {
    .df1 <- .df1 %>% union_all(.df2)
  }
  return (.df1)
}

cluster_count <- 6
titles <- NULL
#------- main()
for (.date_range in date_ranges) {
  print(.date_range)
  df3 <- df2 %>% 
    filter(date_range == .date_range) %>% 
    filter(!grepl('German', lang)) %>% 
    # rename(language = lang) %>% 
    select(-date_range)
  clusters <- create_clusters(df3)
  titles <- add_df(titles, 
                   clusters$titles %>% 
                     mutate('phase' = .date_range)
                   )
  draw_clusters(clusters, .date_range)
}

write_csv(
  titles, 
  sprintf('data_raw/jokai/clusters-%s.csv', cluster_count)
)

