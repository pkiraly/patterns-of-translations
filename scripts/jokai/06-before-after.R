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
    rename(before = `1801-1944`, after = `1945-2025`) %>% 
    filter(before > 0 & after > 0)
  
  df2 %>% 
    ggplot(aes(x = before, y = after)) +
      geom_point() +
      geom_abline(color = 'cornflowerblue') +
      geom_text_repel(aes(label = title), color = 'grey') +
      theme_bw() +
      labs(
        title = '1945 előtt és után is kiadott Jókai-fordítások',
        subtitle = paste(
          .subtitle,
          sprintf(
            '(%d mű, %s kiadás)',
            nrow(df2),
            sum(df2$before) + sum(df2$after)
          )
        ),
        x = 'kiadások száma 1945 előtt',
        y = 'kiadások száma 1945 után'
      ) +
      xlim(0, 55) +
      ylim(0, 55)
  
  ggsave(sprintf('images/jokai/before-after-%s.png', .suffix),
         width = 8, height = 7, units = 'in', dpi = 300)
}

prepare_df <- function(.df) {
  .df %>% 
    select(title = orig_title,
         lang = targ_lan_n,
         start = year_n,
         region) %>%
    filter(title != 'Források:' & !is.na(lang) & !is.na(start)) %>% 
    mutate(
      date_range = cut(start, breaks = years, labels = labels)
    ) %>% 
    select(-c(start, region))
}

#' prepare cuts
years <- c(1800, 1944, 2025)
labels <- c()
for (i in 1:(length(years)-1)) {
  labels <- c(labels, sprintf("%d-%d", years[i]+1, years[i+1]))
}

#' create data frame

df <- readRDS('data_raw/jokai.rds')

df_no_hun <- df %>% 
  filter(is.na(country) | country != 'Hungary')

df %>% 
  filter(is.na(country)) %>% 
  count(city_n) %>% 
  arrange(desc(n))

%>% 
  pull(city_n)

before_after_plot(
  prepare_df(df_no_hun), 'without-hungary', 'a magyarországi kiadások nélkül')
before_after_plot(
  prepare_df(df), 'with-hungary', 'a magyarországi kiadásokkal')

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


