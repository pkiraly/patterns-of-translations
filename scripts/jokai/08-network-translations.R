#' calculate network

library(tidyverse)
library(ggrepel)
library(reshape2)
library(ggpubr)
library(igraph)

get_multilingual_works <- function(.df) {
  .df %>%
    filter(!is.na(title)) %>% 
    count(title, lang) %>% 
    count(title) %>% 
    filter(n > 1) %>% 
    pull(title)
}

create_empty_df <- function() {
  as_tibble(
    data.frame(
      c1 = character(),
      c2 = character(),
      score = numeric(),
      year = numeric()
    )
  )
}

create_translation_sequence <- function(.df, .titles) {
  ratios_all <- create_empty_df()
  for (.title in .titles) {
    ratios <- create_empty_df()
    translation_of_work <- .df %>% 
      filter(title == .title) %>% 
      select(year, lang) %>% 
      arrange(year) %>% 
      distinct()
    # print(translation_of_work)
    # print(l)
    ratios_list <- by(
      translation_of_work, 
      seq_len(nrow(translation_of_work)), 
      function(row) {
        .year <- row$year
        .lang <- row$lang
        equals <- translation_of_work %>% 
          filter(year == .year & lang != .lang)
        if (nrow(equals) > 0) {
          # equals
          langs <- equals %>% select(lang) %>% distinct() %>% pull()
          equals <- as_tibble(data.frame(c1 = langs, c2 = .lang, score = 0.5, year = .year))
          ratios <- ratios %>% union_all(equals)
          equals <- as_tibble(data.frame(c1 = .lang, c2 = langs, score = 0.5, year = .year))
          ratios <- ratios %>% union_all(equals)
        }
        
        effect <- translation_of_work %>% 
          filter(year > .year & year < (.year + 10) & lang != .lang) %>% 
          select(lang, year) %>% 
          distinct()
        if (nrow(effect) > 0) {
          # effect
          effect2 <- effect %>% mutate(c1 = .lang, score = 1) %>% 
            rename(c2 = lang) %>% 
            select(c1, c2, score, year)
          ratios <- ratios %>% union_all(effect2)
        }
        ratios
      }
    )
    for (ratios_df in ratios_list) {
      if (nrow(ratios_df) > 0) {
        ratios <- ratios %>% union_all(ratios_df)
      }
    }
    
    # remove duplicates
    deduplication <- ratios %>% distinct() %>% 
      arrange(c1, c2, year) %>% 
      group_by(c1, c2) %>% 
      mutate(diff = year - lag(year)) %>% 
      ungroup() %>% 
      filter(is.na(diff) | diff > 10) %>% 
      arrange(year) %>% 
      select(-diff)
    
    # find false positives (that already had translations in 10 years)
    false_positive <- translation_of_work %>% 
      inner_join(deduplication, by = join_by(lang == c2),
                 relationship = "many-to-many") %>% 
      filter(year.x < year.y & year.x + 10 > year.y) %>% 
      select(c1, c2 = lang, score, year = year.y)
    
    # remove false positives
    ratios <- anti_join(deduplication, false_positive)
    
    ratios_all <- ratios_all %>% union_all(ratios)
  }
  ratios_all
}

filter_by_weight <- function(.df, .limit, show_all = FALSE) {
  print(sprintf('filter_by_weight) .limit=%d', .limit))
  
  # edges <- .df  %>% 
  #  filter(weight >= .limit)
  #  head(.limit)
  
  .weight <- .df %>% slice_head(n = .limit) %>% 
    slice_tail(n = 1) %>% pull(weight)
  
  edges <- .df  %>% 
    filter(weight >= .weight)
  
  print(edges)
  if (show_all) {
    nodes <- tibble(n = c(edges$c1, edges$c2)) %>%
      distinct() %>% pull()
    .df %>% 
      filter(c1 %in% nodes & c2 %in% nodes)
  } else {
    edges
  }
}

prepare_data <- function(.df) {
  titles <- get_multilingual_works(.df)
  ratios_all <- create_translation_sequence(.df, titles)
  
  ratios_sum <- ratios_all %>% 
    group_by(c1, c2) %>% 
    summarise(weight = sum(score)) %>% 
    ungroup() %>% 
    arrange(desc(weight)) %>% 
    mutate(rank = row_number())
  ratios_sum %>% tail()
  
  ratios_sum2 <- ratios_sum %>% 
    mutate(d1 = c2, d2 = c1) %>% 
    select(rank, d1, d2, weight)
  
  ratios_sum3 <- ratios_sum %>% 
    left_join(ratios_sum2, join_by(c1 == d1, c2 == d2)) %>% 
    mutate(
      weight.y = ifelse(is.na(weight.y), -Inf, weight.y),
      dir = ifelse(
        weight.x > weight.y,
        'blue',
        ifelse(weight.x == weight.y,
               'darkgreen',
               'cornflowerblue'))) %>% 
    select(-c(rank.x, rank.y, weight.y)) %>% 
    rename(weight = weight.x)
}

draw_plot <- function(.df, .limit = 30) {
  print(sprintf('draw_plot) .limit=%d', .limit))
  edges <- filter_by_weight(.df, .limit, FALSE)
  nodes <- tibble(n = c(edges$c1, edges$c2)) %>% distinct() %>%
    pull()
  net <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
  
  max_weight <- max(E(net)$weight)
  #'-----
  E(net)$color <- E(net)$dir
  # E(net)$label <- E(net)$weight
  E(net)$label <- paste0(E(net)$weight, "\n")
  E(net)$width <- 6 * (E(net)$weight / max_weight)
  # E(net)$width <- log(50*((E(net)$weight) / max_weight))
  
  par(mar = c(0, 0, 0, 0)) # set margin
  plot(net, 
       rescale=T, 
       edge.color=E(net)$color, 
       # edge.arrow.size=.6 * ((100 - length(V(net)))/100),
       # edge.arrow.size=(E(net)$width),
       edge.arrow.size=.9,
       # edge.arrow.size=E(net)$width,
       edge.curved=.3,
       edge.label.cex=1.2,
       edge.label.color='maroon',
       # edge.label.dist=0,
       # edge.label.degree=pi,
       # edge.label.dist=15,
       # edge.label.degree=pi/2, 
       vertex.label.distance=200,
       vertex.label.font=1,
       vertex.label.color='black',
       vertex.size=15,
       vertex.frame.color='lightgrey',
       vertex.color='white',
       vertex.label.cex=1.80
  )
}

get_translations <- function(.df) {
  .df %>% arrange(title, lang, year) %>% 
    group_by(title, lang) %>% 
    slice_head(n = 1) %>% 
    ungroup() %>% 
    arrange(title, lang, year)
}

#' --- main() -------------------------
df <- readRDS('data_raw/jokai.rds')
df_lang <- df %>% 
  filter(!is.na(orig_title) & !is.na(year_n) & !is.na(targ_lan_n)) %>% 
  select(title = orig_title, year = year_n, lang = targ_lan_n, country) %>% 
  mutate(
    lang = ifelse(between(year, 1945, 1989)
                  & lang == 'német' 
                  & !is.na(country) & country == 'GDR', 
                  'német\nDDR', lang),
    lang = ifelse(lang == 'német' 
                  & !is.na(country) 
                  & country == 'Hungary',
                  'német\nHU', lang),
    lang = ifelse(
      lang == 'angol',
      ifelse(!is.na(country) & country == 'UK',
             'angol\nUK',
             ifelse(!is.na(country) & country == 'USA',
                    'angol\nUS',
                    ifelse(!is.na(country) & country == 'Hungary',
                           'angol\nHU',
                           'angol\negyéb')
             )
      ),
      lang
    )
  ) 

df2 <- df_lang %>% filter(year >= 1945)

df_translations <- get_translations(df2)
network_df <- prepare_data(df_translations)

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
