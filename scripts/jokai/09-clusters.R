library(tidyverse)
library(skmeans)
require("cluster")
library(ggpubr)
source('scripts/jokai/common-functions.R')

#' the minimum number languages a work is translated to
min_languages <- 3
#' the number of clusters
cluster_count <- 6
#' calulate with ratio or calculate with frequency
calculate_with_ratio <- TRUE
#' deadline
deadline <- 1920

df <- readRDS('data_raw/jokai.rds')

years <- c(1800, 1905, 1944, 1989, 2025)
labels <- c()
for (i in 1:(length(years)-1)) {
  labels <- c(labels, sprintf("%d-%d", years[i]+1, years[i+1]))
}

df2 <- df %>% 
  select(orig_title, lang = targ_lan_n, start = year_n, country) %>% 
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

df3 <- df2 %>% 
  filter(title %in% selected_titles)

if (calculate_with_ratio == TRUE) {
  df_dates <- df3 %>% 
    count(title, date_range) %>% 
    group_by(title) %>% 
    reframe(
      total = sum(n),
      n = n,
      date_range = date_range,
      value = n / total,
    ) %>% 
    select(-c(total, n)) %>% 
    pivot_wider(
      id_cols = c(title),
      names_from = date_range, values_from = value, # p,
      values_fill = 0)
} else {
  date_range_maximums <- df3 %>% 
    count(title, date_range) %>% 
    group_by(date_range) %>% 
    summarise(max = max(n))
  
  df_dates <- df3 %>% 
    count(title, date_range) %>% 
    left_join(date_range_maximums) %>% 
    mutate(value = n / max) %>% 
    select(-c(max, n)) %>% 
    pivot_wider(
      id_cols = c(title),
      names_from = date_range, values_from = value, # p,
      values_fill = 0)
}

if (calculate_with_ratio == TRUE) {
  df_langs <- df3 %>% 
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
} else {
  df_langs <- df3 %>% 
    count(title, lang) %>%
    pivot_wider(id_cols = c(title),
                names_from = lang, values_from = n,
                values_fill = 0)
}

#' era clusters
#' -----------------------------

dates_matrix = as.matrix(df_dates %>% select(-title))
hparty <- skmeans(dates_matrix, cluster_count, control = list(verbose = FALSE))

cluster_ids <- dimnames(hparty$prototypes)[[1]]
lang_clusters <- as_tibble(hparty$prototypes)
lang_clusters$id <- cluster_ids

clusters <- tibble(title = df_dates$title, cluster = hparty$cluster, author = names(hparty$cluster))
clustered_titles <- clusters %>% 
  group_by(cluster) %>% 
  mutate(
    y = row_number(),
  )

p1 <- clustered_titles %>% 
  mutate(
    y2 = max(clustered_titles$y) - (y - 1)
  ) %>% 
  ggplot(aes(x = 1, y = y2)) + 
    geom_text(aes(label=title)) +
    facet_wrap(vars(cluster)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white"))

limit <- 0.2
p2 <- lang_clusters %>% 
  pivot_longer(1:4) %>% 
  mutate(
    name = factor(name),
    color = ifelse(value >= limit, 'maroon', 'grey'),
  ) %>% 
  ggplot(aes(x = name, y = id, size = value*10)) +
  geom_point(aes(color = color), show.legend = FALSE) +
  geom_text(
    aes(label = ifelse(value >= limit, sprintf("%.2f", value), ''), 
        x = as.numeric(name) + 0.1,
        y = as.numeric(id) + 0.1,
    ),
    # angle = 45, 
    hjust=0, vjust = 0,
    size = 2) +
  labs(
    y = 'clusters',
    x = 'date ranges'
  ) +
  theme_bw() +
  scale_size_area() +
  scale_color_identity()

p2
pcommon <- ggarrange(p1, p2, ncol = 2, nrow = 1)
annotate_figure(pcommon, top = text_grob("Clusters by era", face = "bold", size = 14))
ggsave(
  sprintf(
    'images/jokai/clusters-by-era-%d.png', cluster_count),
  width = 12, height = calculateHeight(12),
  units = 'in', dpi = 300)

#'-------------

df_langs %>% filter(title == 'Az arany ember') %>% 
  pivot_longer(-title) %>% 
  arrange(desc(value))

langs_matrix = as.matrix(df_langs %>% select(-title))
hparty <- skmeans(langs_matrix, cluster_count,
                  control = list(verbose = FALSE))

cluster_ids <- dimnames(hparty$prototypes)[[1]]
lang_clusters <- as_tibble(hparty$prototypes)
lang_clusters$id <- cluster_ids

clusters <- tibble(title = df_langs$title, cluster = hparty$cluster, author = names(hparty$cluster))
clustered_titles <- clusters %>% 
  group_by(cluster) %>% 
  mutate(
    y = row_number(),
    maxy = max(y),
    y2 = maxy - (y - 1), 
  )

p1 <- clustered_titles %>% 
  mutate(y2 = max(clustered_titles$y) - (y - 1)) %>% 
  ggplot(aes(x = 1, y = y2)) + 
  geom_text(aes(label=title), size = 3) +
  facet_wrap(vars(cluster)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white"))

limit <- 0.1
p2 <- lang_clusters %>% 
  pivot_longer(1:(ncol(lang_clusters)-1)) %>% 
  mutate(
    id = factor(id),
    name1 = factor(name),
    color = ifelse(value >= limit, 'maroon', 'grey'),
  ) %>% 
  ggplot(aes(x = name1, y = id)) +
  geom_point(aes(color = color, size = value), show.legend = FALSE) +
  geom_text(
    aes(label = ifelse(value >= limit, sprintf("%s (%.2f)", name, value), ''), 
        #x = as.numeric(name1) + 0.5,
        y = as.numeric(id) + 0.1,
    ),
    angle = 60, hjust=0, vjust = 0,
    size = 2) +
  labs(
    y = 'cluster',
    x = 'languages',
    size = '',
  ) +
  scale_color_identity() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1, size=6)) +
  scale_size_area()
  # scale_y_discrete(limits=c(1, 5), breaks = seq(1, 5))

p2
title <- paste0("Clusters by language",
                ifelse(
                  deadline != FALSE,
                  sprintf(' (-%s)', deadline),
                  ''))
pcommon <- ggarrange(p1, p2, ncol = 2, nrow = 1)
annotate_figure(pcommon, top = text_grob(title, face = "bold", size = 14))
ggsave(
  sprintf(
    'images/jokai/clusters-by-language-%d%s.png',
    cluster_count,
    ifelse(deadline != FALSE, paste0('-', deadline), '')
  ),
  width = 12, height = calculateHeight(12),
  units = 'in', dpi = 300)

