#' calculate similarities by era and languages

library(tidyverse)
library(ggrepel)
library(reshape2)
library(ggpubr)

before_after_plot <- function(.df, .suffix, .subtitle) {
  df2 <- .df %>% 
    count(title, date_range) %>%
    ungroup() %>% 
    pivot_wider(
      names_from = date_range, values_from = n, values_fill = 0) %>% 
    rename(
      before = `1801-1944`,
      after = `1945-2025`
    )
  print(
    df2 %>% 
      filter(before == 0 & after > 0)
  )
  #%>% 
  #  filter(before > 0 & after > 0)
  editions = sum(df2$before) + sum(df2$after)
  
  df3 <- df2 %>% 
    mutate(
      b = before * 100 / sum(before),
      a = after * 100 / sum(after)
    ) %>% 
    select(-c(before, after)) %>% 
    rename(before = b, after = a)
  
  max_ratio <- max(df3$before, df3$after)
  df3 %>% 
    ggplot(aes(x = before, y = after)) +
    geom_jitter() +
    geom_abline(color = 'cornflowerblue') +
    geom_text_repel(aes(label = title), color = 'grey') +
    theme_bw() +
    labs(
      title = 'Jókai translations published both before and after 1945',
      subtitle = paste(
        .subtitle,
        sprintf(
          '(%d works, %s editions)',
          nrow(df2),
          editions
        )
      ),
      x = 'before 1945 (in % of the editions)',
      y = 'after 1945 (in % of the editions)'
    )
  #+
    # xlim(0, max_ratio) +
    # ylim(0, max_ratio)
  
  ggsave(sprintf('images/jokai/before-after-%s.png', .suffix),
         width = 8, height = 7, units = 'in', dpi = 300)
}

prepare_df <- function(.df) {
  filtered <- .df %>% 
    select(title = orig_title,
           lang = targ_lan_n,
           start = year_n,
           genre,
           region) %>%
    filter(title != 'Források:' & !is.na(lang) & !is.na(start)) %>% 
    mutate(
      date_range = cut(start, breaks = years, labels = labels)
    ) %>% 
    select(-c(start, region))
  
  
  genres <- filtered %>% 
    count(genre)
  print(genres)
  return(filtered)
}

#' prepare cuts
years <- c(1800, 1944, 2025)
labels <- c()
for (i in 1:(length(years)-1)) {
  labels <- c(labels, sprintf("%d-%d", years[i]+1, years[i+1]))
}
genres <- c('dráma', 'elbeszélés', 'elbeszélések', 'novella', 'novellák', 'novellák.')

#' create data frame
df <- readRDS('data_raw/jokai.rds') %>% 
  filter(!genre %in% genres)


df_no_hun <- df %>% 
  filter(is.na(country) | country != 'Hungary')

df %>% 
  filter(is.na(country)) %>% 
  count(city_n) %>% 
  arrange(desc(n))

df_no_hun

before_after_plot(
  prepare_df(df_no_hun), 'without-hungary', 'excluding Hungarian editions')
before_after_plot(
  prepare_df(df), 'with-hungary', 'including Hungarian editions')

#'---
#'
prepare_df(df)

df

df_freq <- df %>% 
  select(title = orig_title,
         lang = targ_lan_n,
         start = year_n,
         region,
         orig_pub_yr) %>%
  filter(title != 'Források:' & !is.na(lang) & !is.na(start)) %>% 
  mutate(
    date_range = cut(start, breaks = years, labels = labels),
    # min1 = min(start),
  ) %>% 
  # rename(before = `1801-1944`, after = `1945-2025`) %>% 
  # filter(before > 0 & after > 0) %>% 
  group_by(title, date_range) %>% 
  summarize(
    min = ifelse(min(start) <= 1944, orig_pub_yr, 1945),
    max = ifelse(max(start) <= 1944, 1944, 2025),
    # min2 = min(start),
    # max = max(start),
    r = max - min,
    n = n(),
    freq = n / r,
  ) %>% 
  ungroup() %>% 
  select(-c(min, max, r, n))

titles <- df_freq %>% 
  count(title, date_range) %>%
  ungroup() %>% 
  pivot_wider(
    names_from = date_range, values_from = n, values_fill = 0) %>% 
  rename(before = `1801-1944`, after = `1945-2025`) %>% 
  filter(before > 0 & after > 0) %>% 
  pull(title)

df_freq %>% 
  filter(title %in% titles) %>% 
  pivot_wider(names_from = date_range, values_from = freq) %>% 
  rename(before = `1801-1944`, after = `1945-2025`) %>% 
  ggplot(aes(x = before, y = after)) +
  geom_point() +
  geom_text_repel(aes(label = title)) +
  geom_abline(color = 'cornflowerblue') +
  labs(
    title = 'Frequency of publications before and after 1945',
    subtitle = 'publications per year',
    x = 'before 1945 (in years)',
    y = 'after 1945 (in years)',
  ) +
  theme_bw()

ggsave('images/jokai/before-after-publ-per-year.png',
       width = 12, height = calculateHeight(12), units = 'in', dpi = 300)

df3 <- df2 %>% 
  count(title, date_range) %>%
  ungroup() %>% 
  pivot_wider(names_from = date_range, values_from = n, values_fill = 0) %>% 
  rename(before = `1801-1944`, after = `1945-2025`) %>% 
  mutate(
    total = before + after,
    title = sprintf('%s (%d)', title, total)
  ) %>% 
  arrange(desc(total))

hun_both <- df3 %>% filter(before > 0 & after > 0) %>% pull(title)
hun_before <- df3 %>% filter(before > 0 & after == 0) %>% pull(title)
hun_after <- df3 %>% filter(before == 0 & after > 0) %>% pull(title)

sprintf('both: %s', paste(hun_both, collapse = '; '))
sprintf('before: %s', paste(hun_before, collapse = '; '))
sprintf('after: %s', paste(hun_after, collapse = '; '))


