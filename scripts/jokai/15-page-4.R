library(tidyverse)

df <- readRDS('data_raw/jokai.rds')
df2 <- df %>% 
  filter(orig_title != 'Források:') %>% 
  arrange(target_title, year_n) %>% 
  group_by(target_title, targ_lan_n, world) %>%
  mutate(
    nn = row_number(),
    type = ifelse(row_number() == 1,'translation','republication')
  ) %>% 
  ungroup() %>% 
  mutate(
    date_group = floor((year_n - 1850) / 5),
    date_group = 1850 + (date_group * 5),
    date_group = sprintf('%s-%s', date_group, date_group+4)
  ) %>% 
  filter(year_n < 2010)

df2 %>% 
  count(date_group, type) %>% 
  ggplot(aes(x = date_group, y = n, fill=type)) +
  geom_col() +
  labs(
    y = 'Number of Translations',
    x = 'Publication Date',
    fill = '',
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    legend.position.inside = c(0.8, 0.9),
    legend.direction = 'horizontal',
    
  ) +
  guides(
    fill = guide_legend(position = "inside")
  )
ggsave(
  'images/jokai/translation-vs-republication.png',
  width = 8, height = 4,
  units = 'in', dpi = 300)

