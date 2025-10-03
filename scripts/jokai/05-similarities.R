#' calculate similarities by era and languages

library(tidyverse)
library(ggrepel)
library(reshape2)
library(ggpubr)

#' prepare cuts
years <- c(1800, 1905, 1944, 1989, 2025)
labels <- c()
for (i in 1:(length(years)-1)) {
  labels <- c(labels, sprintf("%d-%d", years[i]+1, years[i+1]))
}

#' create data frame

df <- readRDS('data_raw/jokai.rds')

df2 <- df %>% 
  select(title = orig_title,
         lang = targ_lan_n,
         start = year_n,
         region) %>%
  filter(title != 'Források:' & !is.na(lang) & !is.na(start)) %>% 
  mutate(
    date_range = cut(start, breaks = years, labels = labels)
  ) %>% 
  select(-c(start, region))

maxes <- df2 %>% 
  count(title, date_range) %>% 
  group_by(date_range) %>% 
  summarise(max = max(n))

df_dates <- df2 %>% 
  count(title, date_range) %>% 
  left_join(maxes) %>% 
  # mutate(p = n / max) %>% 
  select(-c(max)) %>% 
  pivot_wider(
    id_cols = c(title),
    names_from = date_range, values_from = n, # p,
    values_fill = 0)

df_dates


df_langs <- df2 %>% 
  count(title, lang) %>%
  pivot_wider(id_cols = c(title),
              names_from = lang, values_from = n,
              values_fill = 0)

df_matrix <- df_dates %>% 
  inner_join(df_langs, by = c('title'))
df_matrix

distances <- dist(df_dates, method = "euclidean",
     diag = TRUE, upper = TRUE)

distances


df_distances <- tibble(melt(as.matrix(distances), varnames = c("row", "col")))
head(df_distances)

df_distances %>% 
  filter(col == 66)

df_distances %>% 
  # filter(!col %in% c(66, 101, 36, 147, 52, 153, 96)) %>%  
  # filter(!row %in% c(66, 101, 36, 147, 52, 153, 96)) %>%  
  filter(row < col) %>% 
  # filter(value > 18) %>% 
  group_by(row) %>% 
  arrange(value) %>% 
  slice_head(n = 5) %>% 
  mutate(
    t1 = df_matrix$title[row],
    t2 = df_matrix$title[col]
  ) %>% 
  ungroup() %>% 
  arrange(desc(value)) %>% 
  print(n = Inf)

str(tibble(df_distances))

df_distances %>% 
  filter(row == 66) %>% 
  mutate(
    t2 = df_matrix$title[col]
  ) %>% 
  arrange(value) %>% 
  head(6) %>% 
  left_join(df_dates, by = c('t2' = 'title'))

df_matrix$title[66]

#  `1801-1905` `1906-1944` `1945-1989` `1990-2025`
df_dates %>% 
  arrange(desc(`1801-1905`)) %>% 
  select(title) %>% 
  head()

df_distances %>% 
  ggplot(aes(x = row, y = col, alpha = value)) +
  geom_point()

#' ------------
#' Classical multidimensional scaling (MDS)
#' or principal coordinates analysis
#' ------------

distances1 <- dist(df_dates, method = "euclidean",
                  diag = TRUE, upper = TRUE)

fit <- cmdscale(distances1, eig=TRUE, k=2) # k is the number

# plot solution
p1 <- tibble(
  title = df_dates$title, 
  x = fit$points[,1], 
  y = fit$points[,2]
  ) %>% 
  ggplot(aes(x = x, y = y)) +
    geom_point() + 
    geom_text_repel(aes(label = title)) +
    labs(
      title = 'Jókai művek hasonlósága a kiadási korszakok alapján',
      # subtitle = 'korszakhatárok: 1905, 1945, 1989',
      x = NULL,
      y = NULL,
    ) + 
    theme_bw()
p1
ggsave('images/jokai/era-similarity.png',
       width = 8, height = 8, units = 'in', dpi = 300)

distances2 <- dist(df_langs, method = "euclidean",
                  diag = TRUE, upper = TRUE)
fit <- cmdscale(distances2, eig=TRUE, k=2) # k is the number

p2 <- tibble(
  title = df_langs$title, 
  x = fit$points[,1], 
  y = fit$points[,2]) %>% 
  ggplot(aes(x = x, y = y)) +
    geom_point() + 
    geom_text_repel(aes(label = title)) +
    labs(
      title = 'Jókai művek hasonlósága a célnyelvek alapján',
      x = NULL, y = NULL,
    ) +
    theme_bw()
p2
ggsave('images/jokai/language-similarity.png',
       width = 8, height = 8, units = 'in', dpi = 300)

distances3 <- (distances1 + distances2) / 2
distances3
fit <- cmdscale(distances3, eig=TRUE, k=2) # k is the number

p3 <- tibble(
  title = df_langs$title, 
  x = fit$points[,1], 
  y = fit$points[,2]) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point() + 
  geom_text_repel(aes(label = title)) +
  labs(
    title = 'Jókai művek hasonlósága kiadási korszakok és célnyelvek alapján',
    x = NULL, y = NULL,
  ) +
  theme_bw()
p3
ggsave('images/jokai/similarity-mean.png', plot = p3,
       width = 8, height = 8, units = 'in', dpi = 300)

p_all <- ggarrange(p1, p2, p3,
          # labels = c("A", "B", "C"),
          ncol = 3
          # nrow = 2
          )
ggsave('images/jokai/similarity-all.png', plot = p_all,
       width = 21, height = 7, units = 'in', dpi = 300)


distances1
distances2


#' TODO
#' - compose the two images to one large
#' - weights of categories