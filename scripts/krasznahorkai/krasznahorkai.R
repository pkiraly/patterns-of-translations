library(tidyverse)
# library(paletteer)
library(RColorBrewer)

world2 <- readRDS('data_raw/post1930-with-regions.rds')
world2
names(world2)
world2 %>% 
  filter(grepl("KRASZNA", author)) %>% 
  select(id, flagged, targ_lan_n, target_title, orig_title, year_n, city_n, isbn) %>% 
  arrange(targ_lan_n, orig_title, year_n) %>% 
  select(targ_lan_n, orig_title, year_n, city_n) %>% 
  print(n = Inf)

df_magyar <- read_csv('~/Documents/research/kisery/krasznahorkai-magyarul.csv')
df_magyar <- df_magyar %>% 
  mutate(title = ifelse(
    title == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
    "Északról hegy...",
    title)) %>% 
  rename(year_orig = year)

df_english <- read_csv('~/Documents/research/kisery/krasznahorkai-english-titles.csv')
df_english <- df_english %>% 
  mutate(
    hungarian = ifelse(
    hungarian == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
    "Északról hegy...",
    hungarian),
    english = ifelse(
      english == "A Mountain to the North, a Lake to the South, Paths to the West, a River to the East",
      "A Mountain to the North, ...",
      english),
    english = ifelse(
      english == "Destruction and Sorrow beneath the Heavens",
      "Destruction and Sorrow ...",
      english),
  )

df_english

df <- read_csv('~/Documents/research/kisery/krasznahorkai-normalized.csv')
nrow(df)
df <- df %>% 
  distinct() %>% 
  group_by(lang, title) %>% 
  mutate(y = min(year)) %>% 
  ungroup() %>% 
  filter(year == y)

df <- df %>% 
  # separate_rows(years, sep ="\\|", convert = TRUE) %>% 
  # rename(year = years) %>% 
  mutate(title = 
    ifelse(
      title == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
      "Északról hegy...",
      ifelse(
        title == "Az utolsó farkas / Herman, a vadőr",
        "Az utolsó farkas",
        title))
  ) %>% 
  left_join(df_magyar, by = "title") %>% 
  left_join(df_english, by = join_by(title == hungarian)) %>% 
  mutate(
    english = ifelse(is.na(english), sprintf('[%s]', title), english)
  )
  # mutate(title = sprintf("%s (%d)", title, year_orig))

nrow(df)

titles <- df %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  select(title) %>% 
  unlist(use.names = FALSE)

titles

titles_en <- df %>% 
  group_by(english) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n)) %>% 
  select(english) %>% 
  unlist(use.names = FALSE)
titles_en

v2 <- c(
  'English' = 'angol', 'French' = 'francia',
  'German' = 'német',
  'Croatian' = 'horvát', 'Italian' = 'olasz')

languages <- df %>% 
  count(lang) %>% 
  arrange(desc(n)) %>% 
  filter(n > 5) %>% 
  select(lang) %>% 
  unlist(use.names = FALSE)

languages

lang2 <- names(v2)[match(df$lang, v2)]
.levels <- c(names(v2), 'all other languages')
lang2 <- ifelse(is.na(lang2), 'all other languages', lang2)
df$lang2 <- lang2
df$lang2 <- factor(lang2, levels = .levels)

df %>% 
  mutate(
    title = factor(title, levels = titles),
    top_lang = ifelse(lang %in% languages, lang, "egyéb"),
    lang3 = factor(lang2, levels = .levels),
  ) %>% 
  arrange(desc(title), year) %>%
  ggplot(aes(y = fct_reorder(title, desc(title)),
             x = year,
             color = lang3)) +
  geom_jitter(height = 0.2) +
  geom_vline(xintercept = 2015, color = "cornflowerblue") +
  geom_vline(xintercept = 2019, color = "cornflowerblue") +
  labs(
    title = "Krasznahorkai László idegen nyelven",
    subtitle = "2015: Nemzetközi Man Booker-díj, 2019: amerikai Nemzeti Könyvdíj",
    x = "A fordítás megjelenési éve",
    y = "A regény címe",
    color = "nyelvek",
    shape = ""
  ) +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1, hjust=1)) +
  scale_x_continuous(breaks = seq(1980, 2025, 5),
                     minor_breaks = seq(1980, 2025, 1),
                     limits = c(1984, 2025))

df %>% 
  select(title, year_orig, genre) %>%
  distinct() %>% 
  mutate(title = sprintf("%s%s", title, 
                         ifelse(!is.na(genre) & genre == "regény", "*", "")))

titles <- df %>% 
  select(title, year_orig, genre) %>%
  distinct() %>% 
  mutate(title = sprintf(
    "%s%s",
    title, 
    ifelse(!is.na(genre) & genre == "regény", "*", ""))) %>% 
  select(-genre) %>% 
  group_by(year_orig) %>% 
  mutate(title = paste(title, collapse = "\n")) %>% 
  ungroup() %>% 
  distinct() %>% 
  mutate(title = sprintf("%s (%d)", title, year_orig))

titles

titles_en <- df %>% 
  select(english, year_orig, genre) %>%
  distinct() %>% 
  mutate(english = sprintf(
    "%s%s",
    english, 
    ifelse(!is.na(genre) & genre == "regény", "*", ""))) %>% 
  select(-genre) %>% 
  mutate(english = sprintf("%s (%d)", english, year_orig)) %>%   group_by(year_orig) %>% 
  mutate(english = paste(english, collapse = "\n")) %>% 
  ungroup() %>% 
  distinct()# %>% 
  # mutate(english = sprintf("%s (%d)", english, year_orig))
titles_en

df %>%
  mutate(
    top_lang = ifelse(lang %in% languages, lang, "egyéb"),
    year_orig = ifelse(
                  title %in% c("A Manhattan-terv", 'ÁllatVanBent'),
                  year_orig + 0.3,
                  ifelse(
                    title %in% c('Aprómunka egy palotáért', 'Számla'),
                    year_orig - 0.3,
                    year_orig
                  )
                ),
  ) %>% 
  ggplot(aes(x = year, y = year_orig, color = lang2)) +
  # geom_point() +
  geom_jitter(height = 0.2, width = 0.3) +
  geom_abline(color = "#cccccc") +
  geom_vline(xintercept = 2015, color = "#cccccc") +
  geom_vline(xintercept = 2019, color = "#cccccc") +
  scale_y_continuous(breaks = titles$year_orig, labels = titles$title,
                     minor_breaks = seq(1980, 2025, 1),) +
  scale_x_continuous(breaks = seq(1980, 2025, 5),
                     minor_breaks = seq(1980, 2025, 1),
                     limits = c(1984, 2025)) +
  labs(
    title = "Krasznahorkai László idegen nyelven",
    subtitle = "*: regény; 2015: Nemzetközi Man Booker-díj; 2019: amerikai Nemzeti Könyvdíj",
    x = "A fordítás megjelenési éve",
    y = "A mű címe",
    color = "nyelvek",
    shape = "",
    caption = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1, hjust=1))

ggsave('images/krasznahorkai7.png',
       width = 12, height = 8, units = 'in', dpi = 300)


df %>% select(english) %>% distinct()

df %>%
  mutate(
    top_lang = ifelse(lang %in% languages, lang, "egyéb"),
    year_orig = ifelse(
      english %in% c("The Manhattan Project", 'AnimalInside'),
      year_orig + 0.3,
      ifelse(
        english %in% c('Spadework for a Palace', 'The Bill'),
        year_orig - 0.3,
        year_orig
      )
    ),
    alph = ifelse(lang == 'all other languages', 0.4, 1),
  ) %>% 
  arrange(desc(lang), desc(year)) %>% 
  ggplot(aes(x = year, y = year_orig, color = lang2)) +
  # geom_point() +
  geom_abline(color = "#cccccc") +
  geom_vline(xintercept = 2015, color = "#cccccc") +
  geom_vline(xintercept = 2019, color = "#cccccc") +
  geom_jitter(height = 0.2, width = 0.3, alpha = 0.7) +
  scale_y_continuous(
    breaks = titles_en$year_orig,
    labels = titles_en$english,
    minor_breaks = seq(1980, 2025, 1)) +
  scale_x_continuous(
    breaks = seq(1980, 2025, 5),
    minor_breaks = seq(1980, 2025, 1),
    limits = c(1984, 2025)) +
  labs(
    title = "Translations of László Krasznahorkai's works",
    subtitle = "*: novel; 2015: Man Booker International Prize; 2019: National Book Award for Translated Literature",
    x = "The first publication of the translation",
    y = "Literary work",
    color = "languages",
    shape = "",
    caption = ""
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(
    angle = 45, vjust = 1, hjust=1)) +
  # scale_color_paletteer_d("tvthemes::AirNomads") +
  # scale_colour_manual(values = c('maroon', '#FF9933FF', 'darkgreen', '#8B5B45FF', '#87AFD1FF', '#DBC5A0FF')) +
  scale_colour_manual(values = c(brewer.pal(5,"Set1"), '#DBC5A0FF'))

ggsave('images/krasznahorkai10.en.png',
       width = 12, height = 8, units = 'in', dpi = 300)
