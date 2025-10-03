#' calculate network

library(tidyverse)
library(ggrepel)
library(reshape2)
library(ggpubr)
library(igraph)
source('scripts/jokai/network-functions.R')

get_translations <- function(.df) {
  .df %>% arrange(title, lang, year) %>% 
    group_by(title, lang) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>% 
    arrange(title, lang, year)
}

#' --- main() -------------------------
df <- readRDS('data_raw/jokai.rds')
df_lang1 <- df %>% 
  filter(!is.na(orig_title) & !is.na(year_n) & !is.na(targ_lan_n)) %>% 
  select(title = orig_title, year = year_n, lang = targ_lan_n, country)
df_lang <- translate_languages(df_lang1)

df2 <- df_lang %>% filter(year >= 1945)
df_translations <- get_translations(df2)
df_translations
network_df <- prepare_data(df_translations)

png("images/jokai/network-translation-post-1945-all-nodes.png", 1000, 1000)
draw_plot(network_df, FALSE, TRUE)
dev.off()

png("images/jokai/network-translation-post-1945-all-edges.png", 1000, 1000)
draw_plot(network_df, 25, TRUE)
dev.off()

png("images/jokai/network-translation-post-1945.png", 1000, 1000)
draw_plot(network_df, 25)
dev.off()

df2 <- df_lang %>% filter(year < 1945)

df_translations <- get_translations(df2)
network_df <- prepare_data(df_translations)

png("images/jokai/network-translation-pre-1945.png", 1000, 1000)
draw_plot(network_df, 25)
dev.off()

#' END of main

#' --- Network analysis with igraph -------------------------


#' --- Network analysis with hdir -------------------------
# remotes::install_github("taylor-arnold/rpkg", subdir = "hdir")
library(hdir)

ratios_all

nwork <- hdir_network_metrics(ratios_all, directed = TRUE)
node <- nwork$node
edge <- nwork$edge
node
edge

#' indirected:
#' id, x, y, degree, eigen, close, between, cluster, component, component_size
#' directed:
#' id, x, y, degree_out, degree_in, degree_total, eigen, close, between, cluster, component, component_size
#' degree
node %>% arrange(desc(degree_in)) %>% select(id, degree_in)
node %>% arrange(desc(degree_out)) %>% select(id, degree_out)
#' eigen
node %>% arrange(desc(eigen)) %>% select(id, eigen)
#' close
node %>% arrange(desc(close)) %>% select(id, close)
#' between
node %>% arrange(desc(between)) %>% select(id, between)
#' cluster
node %>% arrange(desc(cluster)) %>% select(id, cluster)

edge

#' clusters
node |>
  ggplot(aes(x, y)) +
  geom_segment(
    aes(xend = xend, yend = yend),
    data = edge,
    alpha = 0.1,
    arrow = arrow( length = unit (0.02 , "npc"))
  ) +
  geom_point(aes(color = cluster), size = 2) +
  geom_text_repel(aes(label = id), color = 'black') +
  theme_void()

edge

node |>
  filter(component %in% c(1)) |>
  select(degree_out, degree_in)

node |>
  filter(component %in% c(1)) |>
  ggplot(aes(x = degree_out, y = degree_in)) +
  geom_point()

node %>% distinct(component)
node |>
  # filter(id != 'német') |>
  ggplot(aes(x = degree_out, y = degree_in)) +
  geom_point() +
  geom_text_repel(
    aes(label = id),
    # data = edge,
    nudge_y = 1,
    nudge_x = -1
  ) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = 'cornflowerblue') +
  theme_bw() + 
  labs(
    x = 'korábbi kiadások',
    y = 'későbbi kiadások',
  )
