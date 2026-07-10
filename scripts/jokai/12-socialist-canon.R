library(tidyverse)
library(stringr)

drawPlot <- function(df2, .title, .subtitle = NULL) {
  max <- max(abs(df2$n), na.rm = TRUE)
  df2  %>% 
    mutate(
      title = ifelse(str_length(title) > 30, paste0(str_sub(title, 1, 30), '...'), title)
    ) %>% 
    ggplot(aes(x = n, y = title, fill=socialist_era)) +
    geom_col() +
    theme_bw() +
    xlim(max * -1, max) +
    labs(
      title = .title,
      subtitle = .subtitle,
      fill = 'socialist era'
    ) +
    scale_fill_discrete(
      breaks = c(FALSE, TRUE),
      labels = c('no', 'yes')
    )
}

savePlot <- function(suffix) {
  file_name <- sprintf(
    'images/jokai/era-comparision-%s.png',
    suffix
  )
  ggsave(
    file_name,
    width = 8, height = 6,
    units = 'in', dpi = 300)
}

df <- readRDS('data_raw/jokai.rds')
df1 <- df %>% 
  select(title = orig_title, lang = targ_lan_n, year = year_n, country) %>% 
  filter(!is.na(year) & !is.na(title)) %>% 
  mutate(
    socialist_era = (year >= 1949 & year <= 1989) 
  ) 

df2 <- df1 %>% 
  filter(lang  == 'cseh') %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))

drawPlot(df2, 'Czech')
savePlot('czech')

df2 <- df1 %>% 
  filter(lang  == 'szlovák') %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))

drawPlot(df2, 'Slovak')
savePlot('slovak')

df2 <- df1 %>% 
  filter(lang  == 'lengyel') %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))
df2 %>% print(n = Inf)

drawPlot(df2, 'Polish')
savePlot('polish')

#' -- román, orosz, bolgár
df2 <- df1 %>% 
  filter(lang  == 'román') %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))
df2 %>% print(n = Inf)

drawPlot(df2, 'Romanian')
savePlot('romanian')

df2 <- df1 %>% 
  filter(lang  == 'orosz') %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))
df2 %>% print(n = Inf)

drawPlot(df2, 'Russian')
savePlot('russian')

df2 <- df1 %>% 
  filter(lang  == 'bolgár') %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))
df2 %>% print(n = Inf)

drawPlot(df2, 'Bulgarian')
savePlot('bulgarian')

#' --- German is different...

names(df)
df2 <- df %>% 
  select(title = orig_title, lang = targ_lan_n, year = year_n,
         country, world, city = city_n) %>% 
  filter(lang  == 'német' & !is.na(year) & !is.na(title)) %>% 
  # filter(year >= 1949 & year <= 1989) %>% 
  filter(!grepl('Hungary', country)) %>% 
  mutate(
    socialist_era = (year >= 1949 & year <= 1989 & country %in% c('GDR')) 
  ) %>% 
  count(title, socialist_era) %>% 
  mutate(n = ifelse(socialist_era,n,n * -1))

drawPlot(df2, 'German', 'all works')
savePlot('german-all')

titles <- df2 %>% 
  pivot_wider(id_cols = title, names_from = socialist_era, names_prefix = 's', values_from = n) %>% 
  filter(sFALSE == -1 & is.na(sTRUE)) %>% pull(title)

df3 <- df2 %>% filter(! (title %in% titles))
df3 %>% filter(n > 0)
drawPlot(df3, 'German', 'selected works')
savePlot('german-selected')

