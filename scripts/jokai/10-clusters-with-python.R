library(tidyverse)
library(ggpubr)
library(eulerr)
source('scripts/jokai/common-functions.R')


py_dir <- '~/git/dh/dnb_trans_networks'

date_ranges <- c('1801-1905', '1906-1944', '1945-1989', '1990-2025')

algs <- c('FastGreedy', 'Louvain', 'Leiden', 'Infomap')
for (alg in algs) {
  images <- c()
  for (.date_range in date_ranges) {
    print(.date_range)
    file <- sprintf(
      'data_raw/jokai/jokai-langs-%s.csv', .date_range)
    df <- read_csv(file, show_col_types = FALSE)
    counts <- df %>% 
      count(language)
    
    f <- sprintf('%s/jokai/%s/community_memberships.csv',
                 py_dir, .date_range)
    print(f)
    df <- read_csv(f, show_col_types = FALSE) %>% rename(FastGreedy = `Fast-greedy`)
    clustered_titles <- df %>%
      left_join(counts, by = join_by(language)) %>%
      mutate(language = sprintf('%s (%s)', language, n)) %>%
      select(title=language, cluster={{alg}}, n) %>% 
      group_by(cluster) %>% 
      mutate(
        y = row_number(),
        y2 = max(y) - (y - 1),
        cluster = sum(n)
      )
    
    p <- clustered_titles %>% 
      ggplot(aes(x = 1, y = y2)) + 
      geom_text(aes(label=title)) +
      facet_wrap(vars(cluster)) +
      theme_void() +
      labs(title = .date_range) +
      theme(plot.background = element_rect(fill = "white"))
    images <- c(images, p)
    
    # title cluster
    if (alg == 'FastGreedy') {
      cluster_file <- sprintf('%s/jokai/%s/plots/title_community.csv',
                              py_dir, .date_range)
      df <- read_csv(cluster_file, show_col_types = FALSE)
      print(df)
      v <- df %>% 
        mutate(cluster = gsub('->', '&', cluster)) %>% 
        count(cluster) %>% 
        pivot_wider(names_from = cluster, values_from = n) %>% 
        unlist()
      vennDiag <- euler(v %>% unlist(), input="disjoint")
      # ~/git/pkiraly/patterns-of-translations/scripts/jokai/10-clusters-with-python.R
      img_file <- sprintf("~/git/patterns-of-translations/images/jokai/title-clusters-by-era-%s.png", .date_range)
      print(img_file)
      png(filename=img_file)
      plot(vennDiag, 
           edges = list(lty = 3),
           quantities = list(type = "counts", font = 3),
           main = sprintf('title clusters (%s)', .date_range))
      dev.off()
    }
  }
  pcommon <- ggarrange(
    images[[1]], images[[2]], images[[3]], images[[4]],
    ncol = 2, nrow = 2)
  title <- sprintf("Language clusters by era (%s)", alg)
  annotate_figure(pcommon, 
                  top = text_grob(title, face = "bold", size = 14))
  # print(pcommon)
  ggsave(
    sprintf(
      'images/jokai/language-clusters-by-era-%s.png', alg),
    width = 12, height = calculateHeight(12),
    units = 'in', dpi = 300)
  
}
#'------- END

df <- read_csv('data_raw/jokai/jokai-langs-1801-1905.csv')
counts <- df %>% 
  count(language)
counts

algs <- c('FastGreedy', 'Louvain', 'Leiden', 'Infomap')

f <- "~/git/dh/dnb_trans_networks/jokai/community_memberships-1801-1905.csv"
df <- read_csv(f) %>% rename(FastGreedy = `Fast-greedy`)
df
a <- 'FastGreedy'
df %>% select({{a}})
df %>% 
  left_join(counts) %>% 
  print(n = Inf)
df %>% 
  group_by(`Fast-greedy`) %>% 
  summarise(
    l = paste(language, collapse = ', ')
  )


clustered_titles <- df %>%
  left_join(counts, by = join_by(language)) %>%
  mutate(language = sprintf('%s (%s)', language, n)) %>% 
  select(title=language, cluster=`Fast-greedy`, n) %>% 
  group_by(cluster) %>% 
  mutate(
    y = row_number(),
    y2 = max(clustered_titles$y) - (y - 1),
    m = sum(n),
    cluster2 = sprintf('%s (%s)', cluster, m)
  )
clustered_titles

p1 <- clustered_titles %>% 
  ggplot(aes(x = 1, y = y2)) + 
  geom_text(aes(label=title)) +
  facet_wrap(vars(cluster2)) +
  theme_void() +
  labs(title = '1801-1905') +
  theme(plot.background = element_rect(fill = "white"))
print(p1)


##----------

img_dir = '/home/pkiraly/git/pkiraly/patterns-of-translations/images/jokai'
for (.date_range in date_ranges) {
  print(.date_range)
  # title cluster
  cluster_file <- sprintf('%s/jokai/%s/plots/title_community.csv',
                            py_dir, .date_range)
  df <- read_csv(cluster_file, show_col_types = FALSE)
  print(df)
  v <- df %>% 
    mutate(cluster = gsub('->', '&', cluster)) %>% 
    count(cluster) %>% 
    pivot_wider(names_from = cluster, values_from = n) %>% 
    unlist()
  vennDiag <- euler(v, input="disjoint")
  print(vennDiag)
  img_file <- sprintf("%s/title-clusters-by-era-%s.png", img_dir, .date_range)
  print(img_file)
  png(filename=img_file)
  plot(vennDiag, 
       edges = list(lty = 3),
       quantities = list(type = "counts", font = 3),
       main = sprintf('title clusters (%s)', .date_range))
  dev.off()
}
# 