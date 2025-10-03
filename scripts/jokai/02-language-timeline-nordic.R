library(tidyverse)
# library(paletteer)
library(RColorBrewer)
library(colorspace)
pdf.options(encoding = 'CP1250')
source('scripts/jokai/common-functions.R')

df <- readRDS('data_raw/jokai.rds')

df <- df %>% mutate(
  t = orig_title,
  t = ifelse(t == 'A szökevény − Árnyképek', 'Árnyképek*', t),
  t = ifelse(t == 'Valahány ház, annyi szokás − Árnyképek', 'Árnyképek*', t),

  t = ifelse(t == 'A Bárdy család − Forradalmi és csataképek 1848. és 1849-ből', 'Csataképek*', t),
  t = ifelse(t == 'Az elesett neje − Forradalmi és csataképek 1848. és 1849-ből', 'Csataképek*', t),
  t = ifelse(t == 'Egy bujdosó naplója − Forradalmi és csataképek 1848. és 1849-ből', 'Csataképek*', t),
  # t = ifelse(t == 'Az elesett neje − Csataképek', 'Csataképek*', t),
  t = ifelse(t == 'A kalózkirály − Hangok a vihar után', 'Hangok a vihar után*', t),
  t = ifelse(t == 'A dagői torony − Targallyak', 'Targallyak*', t),
  t = ifelse(t == 'A jedikulai rab − Targallyak', 'Targallyak*', t),
  t = ifelse(t == 'Az egyhuszasos leány − Targallyak', 'Targallyak*', t),
  t = ifelse(t == 'A rútak rútja − Virradóra', 'Virradóra*', t),
  t = ifelse(t == 'Az utolsó budai basa − Novellák', 'Novellák*', t),
  t = ifelse(t == 'Carinus − Novellák', 'Novellák*', t),
  t = ifelse(t == 'Fortunatus Imre − Hangok a vihar után', 'Hangok a vihar után*', t),
  t = ifelse(t == 'Kedves atyafiak − Népvilág', 'Népvilág*', t),
  t = ifelse(t == 'Egy asszonyi hajszál − Magyar előidőkből', 'Magyar előidőkből*', t),
  t = ifelse(t == 'Párbaj az Istennel − Föld felett és víz alatt', 'Föld felett és víz alatt*', t),
  t = ifelse(t == 'Petki Farkas leányai − Erdélyi képek', 'Erdélyi képek*', t),
  t = ifelse(t == 'Az ezerkettedik éjszaka − Megtörtént regék', 'Megtörtént regék*', t),
  t = ifelse(t == 'A kardvas és a villám − Milyenek a férfiak?', 'Milyenek a férfiak?*', t),
  t = ifelse(t == 'A hittagadó − Milyenek a férfiak', 'Milyenek a férfiak?*', t),
  t = ifelse(t == 'A rabnő − Milyenek a nők?', 'Milyenek a nők?*', t),
  t = ifelse(t == 'Az úrnő − Milyenek a nők?', 'Milyenek a nők?*', t),
  t = ifelse(t == 'Miranda − Milyenek a nők?', 'Milyenek a nők?*', t),
  
  t = ifelse(t == 'A Khánok utódja − Véres könyv', 'Véres könyv*', t),
  t = ifelse(t == 'A gyerkőcz − Véres könyv', 'Véres könyv*', t),
  t = ifelse(t == 'A jó öreg asszony − Életemből II.', 'Életemből II.*', t),

  t = ifelse(t == 'A rémhalász − Mesék és regék', 'Mesék és regék*', t),
  t = ifelse(t == 'Az apja fia − Még egy csokrot', 'Még egy csokrot*', t),
  t = ifelse(t == 'Csodálatos történetek − Halál után', 'Halál után*', t),
  t = ifelse(t == 'Egy szegény asszony története − Szélcsend alatt', 'Szélcsend alatt*', t),

  t = ifelse(t == 'Bolivár − Dekameron I.', 'Dekameron*', t),
  t = ifelse(t == 'A láthatatlan seb − Dekameron I.', 'Dekameron*', t),
  t = ifelse(t == 'A székely adott szava − Dekameron I.', 'Dekameron*', t),
  t = ifelse(t == 'Százszorszépek − Dekameron I.', 'Dekameron*', t),
  t = ifelse(t == 'Az ördög menyasszonya − Valdivia − Dekameron I.', 'Dekameron*', t),
  t = ifelse(t == 'Három közül a legszebb − Dekameron II.', 'Dekameron*', t),
  t = ifelse(t == 'A koldus-gyermek − Dekameron II.', 'Dekameron*', t),
  t = ifelse(t == 'A mennyegző utáni nap − Dekameron II.', 'Dekameron*', t),
  t = ifelse(t == 'A caldaria − Dekameron II.', 'Dekameron*', t),
  t = ifelse(t == 'Tíz millió dollár − Dekameron II.', 'Dekameron*', t),
  t = ifelse(t == 'I love you − Dekameron III.', 'Dekameron*', t),
  t = ifelse(t == 'Mi van a föld alatt? − Dekameron III.', 'Dekameron*', t),
  t = ifelse(t == 'Történetek egy ócska kastélyban − Dekameron III.', 'Dekameron*', t),
  
  t = ifelse(t == 'A Khánok utódja − Véres könyv; A láthatatlan csillag', 'Véres könyv; A láthatatlan csillag*', t),
  t = ifelse(t == 'A tábornok és az asztrál-szellem − Véres könyv', 'Véres könyv*', t),
  
  t = ifelse(t == 'Kelet királynéja − Délvirágok', 'Délvirágok*', t),
  t = ifelse(t == 'Láda! Kérnek! Jössze-e? − Még egy csokrot', 'Még egy csokrot*', t),
  t = ifelse(t == 'Az egyiptusi rózsa − Vadon virágai', 'Vadon virágai*', t),
  t = ifelse(t == 'Sonkolyi Gergely − Vadon virágai', 'Vadon virágai*', t),
  t = ifelse(t == 'Tarka világ − Igaz történetek', 'Igaz történetek*', t),
  
  t = ifelse(t == 'Samyl fiai − Görögtűz', 'Görögtűz*', t),
  t = ifelse(t == 'Riumin − Görögtűz', 'Görögtűz*', t),
  
  t = ifelse(t == 'Óceánia, vagy egy elsüllyedt világrész története', 'Óceánia...', t),
  t = ifelse(t == 'És mégis mozog a föld: Eppur si muove', 'És mégis mozog a föld', t),
  orig_title = t,
) %>% 
  select(-t) %>% 
  mutate(
    targ_lan_n = case_match(
      targ_lan_n,
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
      'szlovák' ~ 'Slovak',
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
    targ_lan_n = ifelse(between(year_n, 1945, 1989)
                  & targ_lan_n == 'German' 
                  & !is.na(country) & country == 'GDR', 
                  paste0('DDR\n', targ_lan_n), targ_lan_n),
    targ_lan_n = ifelse(targ_lan_n == 'German' 
                  & !is.na(country) 
                  & country == 'Hungary',
                  paste0('HU\n', targ_lan_n), targ_lan_n),
    targ_lan_n = ifelse(
      targ_lan_n == 'English',
      ifelse(!is.na(country) & country == 'UK', paste0('UK\n', targ_lan_n),
             ifelse(!is.na(country) & country == 'USA', paste0('US\n', targ_lan_n),
                    ifelse(!is.na(country) & country == 'Hungary', paste0('HU\n', targ_lan_n),
                           targ_lan_n))),
      targ_lan_n
    )
  )

df %>% 
  select(orig_title) %>% 
  filter(grepl(' − ', orig_title)) %>% 
  count(orig_title) %>% 
  arrange(desc(n))

selected_languages <- c('Swedish', 'Finnish', 'Danish')
selected_titles <- df %>%
  filter(!is.na(targ_lan_n) & !is.na(orig_title)) %>%
  filter(!(orig_title %in% c('Források:'))) %>% 
  filter(targ_lan_n %in% selected_languages) %>% 
  select(orig_title) %>% 
  distinct() %>% 
  pull(orig_title)

df2 <- df %>%
  filter(orig_title %in% selected_titles) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  mutate(
    title = sprintf("%s (%d)", orig_title, orig_pub_yr)
  ) %>% 
  arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
  select(orig_title, orig_pub_yr, targ_lan_n, year_n)

multiyear <- df2 %>%
  select(orig_pub_yr, orig_title) %>% 
  distinct() %>% 
  group_by(orig_pub_yr) %>% 
  summarise(n = n()) %>% 
  filter(n > 1)

df3 <- df2 %>%
  select(orig_pub_yr, orig_title) %>% 
  distinct() %>%
  arrange(orig_pub_yr) %>% 
  group_by(orig_pub_yr) %>% 
  mutate(
    id = row_number(), 
    n = n(),
    z = (id - 1) * (1 / n),
    y = orig_pub_yr + z,
    value = rnorm(n = 1),
    title = sprintf("%s (%s)", orig_title, orig_pub_yr)
  )

title_axis <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  select(orig_pub_yr, title = orig_title) %>% 
  distinct() %>% 
  group_by(orig_pub_yr) %>% 
  mutate(title = paste(title, collapse = " − ")) %>% 
  distinct(orig_pub_yr, title) %>% 
  arrange(orig_pub_yr) %>%
  mutate(title = sprintf("%s (%d)", title, orig_pub_yr))

language_colors <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  filter(targ_lan_n %in% selected_languages) %>% 
  count(targ_lan_n) %>% 
  arrange(desc(n)) %>% 
  mutate(
    id = row_number(),
    color = ifelse(id < 8, targ_lan_n, 'others')
  ) %>% 
  select(targ_lan_n, color)

df_final <- df %>%
  filter(orig_title %in% selected_titles) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  left_join(df3, by = join_by(orig_pub_yr, orig_title)) %>% 
  left_join(language_colors, by = join_by(targ_lan_n)) %>% 
  filter(targ_lan_n %in% selected_languages) %>% 
  arrange(orig_pub_yr)
# select(orig_pub_yr, orig_title, y) %>% 
# arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 
df_final
df_final %>%
  ggplot(aes(x = year_n, y = y)) +
  geom_jitter(aes(colour = color), height = 0.1, alpha = 0.7) +
  geom_abline(color = "#cccccc") +
  geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1906, y = max(df_final$orig_pub_yr) + 1, label = "Jókai' death (1905)",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1946, y = max(df_final$orig_pub_yr) + 1, label = "1945",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  # geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
  # annotate("text", x = 1990, y = max(df_final$orig_pub_yr) + 1, label = "1989",
  #          color="cornflowerblue", hjust = "left", size = 10/.pt) +
  scale_y_continuous(
    breaks = title_axis$orig_pub_yr,
    labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
    ) +
  scale_x_continuous(
    breaks = seq(1850, 1989, 10),
    labels = seq(1850, 1989, 10),
    
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  labs(
    title = 'Editions of Jókai\'s translated works in Nordic languages', # "Jókai Mór műveinek fordításai - első kiadások",
    subtitle = '* = only partly translated', # "csak a legalább 4 nyelvre lefordított alkotások",
    x = "Publication year of the translation",
    y = "Work",
    color = "languages",
    # shape = "",
    caption = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(
    values = c(brewer.pal(3, "Set2"), '#DBC5A0FF')
    # values = c(rainbow_hcl(8))
  ) +
  # scale_x_continuous(breaks = seq(1850, 1989, 10)) +
  xlim(1850, 1989)

ggsave('images/jokai/work-timeline-nordic.png',
       width = 12, height = calculateHeight(12), units = 'in', dpi = 300)


#' ----

first_editions <- df %>% 
  filter(!is.na(orig_pub_yr) & orig_title != 'Források:') %>% 
  filter(orig_title %in% selected_titles) %>% 
  select(id, orig_title, orig_pub_yr, targ_lan_n, year_n) %>% 
  arrange(orig_title, targ_lan_n, year_n) %>% 
  group_by(orig_title, targ_lan_n) %>% 
  mutate(kiadas = row_number()) %>% 
  ungroup() %>% 
  filter(kiadas == 1) %>% 
  select(id) %>% 
  pull()

df_final <- df %>%
  filter(orig_title %in% selected_titles) %>% 
  filter(id %in% first_editions) %>% 
  filter(!is.na(orig_pub_yr)) %>% 
  left_join(df3, by = join_by(orig_pub_yr, orig_title)) %>% 
  left_join(language_colors, by = join_by(targ_lan_n)) %>% 
  arrange(orig_pub_yr)
  # select(orig_pub_yr, orig_title, y) %>% 
  # arrange(desc(targ_lan_n), desc(orig_pub_yr)) %>% 

df_final %>% 
  ggplot(aes(x = year_n, y = y)) +
  geom_jitter(aes(colour = color), height = 0.1, alpha = 0.7) +
  geom_abline(color = "#cccccc") +
  geom_vline(xintercept = 1905, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1906, y = max(df_final$orig_pub_yr) + 1, label = "Jókai halála",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1945, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1946, y = max(df_final$orig_pub_yr) + 1, label = "1945",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  geom_vline(xintercept = 1989, color = "cornflowerblue", alpha=0.5) +
  annotate("text", x = 1990, y = max(df_final$orig_pub_yr) + 1, label = "1989",
           color="cornflowerblue", hjust = "left", size = 10/.pt) +
  scale_y_continuous(
    breaks = title_axis$orig_pub_yr,
    labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  scale_x_continuous(
    breaks = seq(1850, 2030, 10),
    # labels = title_axis$title,
    # minor_breaks = seq(1850, 1900, 1)
  ) +
  labs(
    title = 'Translations of Jókai\'s work', # "Jókai Mór műveinek fordításai - első kiadások",
    subtitle = 'only works translated at least 4 languages. * = only partly translated', # "csak a legalább 4 nyelvre lefordított alkotások",
    x = "Publication year of the translation",
    y = "Work",
    color = "languages",
    shape = "",
    caption = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  scale_colour_manual(
    # values = c(brewer.pal(7, "Set2"), '#DBC5A0FF')
    values = c(rainbow_hcl(8))
  ) +
  xlim(1850, 2020)

ggsave('images/jokai/work-timeline-first-editions.png',
       width = 12, height = calculateHeight(12), units = 'in', dpi = 300)
