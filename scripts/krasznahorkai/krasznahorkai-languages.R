library(tidyverse)
# library(paletteer)
library(RColorBrewer)

df_magyar <- read_csv('~/Documents/research/kisery/krasznahorkai/krasznahorkai-magyarul.csv')
df_magyar <- df_magyar %>% 
  mutate(title = ifelse(
    title == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
    "Északról hegy...",
    title)) %>% 
  rename(year_orig = year)
df_magyar

df_english <- read_csv('~/Documents/research/kisery/krasznahorkai/krasznahorkai-english-titles.csv')
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

df <- read_csv('~/Documents/research/kisery/krasznahorkai/krasznahorkai-normalized.csv')
nrow(df)
df <- df %>% 
  mutate(
    title = ifelse(
      title == "Északról hegy, Délről tó, Nyugatról utak, Keletről folyó",
      "Északról hegy...",
      ifelse(
        title == "Az utolsó farkas / Herman, a vadőr",
        "Az utolsó farkas",
        title))
  ) %>% 
  distinct() %>% 
  group_by(lang, title) %>% 
  mutate(y = min(year)) %>% 
  ungroup() %>% 
  filter(year == y)

df %>% print(n = Inf)
df %>% 
  select(-y) %>% 
  write_csv('~/Documents/research/kisery/krasznahorkai/krasznahorkai-normalized-elso-kiadasok.csv')

df <- df_magyar %>% 
  left_join(df, by = "title") %>% 
  left_join(df_english, by = join_by(title == hungarian)) %>% 
  mutate(
    english = ifelse(is.na(english), sprintf('[%s]', title), english)
  )
# mutate(title = sprintf("%s (%d)", title, year_orig))

df %>% filter(is.na(lang))

nrow(df)

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

df %>% select(english) %>% distinct()

lang.en <- c(
  "German" = "német", "English" = "angol", "French" = "francia",
  "Bulgarian" = "bolgár", "Spanish" = "spanyol", "Czech" = "cseh",
  "Polish" = "lengyel", "Japanese" = "japán", "Hebrew" = "héber",
  "Dutch" = "holland", "Croatian" = "horvát", "Romanian" = "román",
  "Italian" = "olasz", "Serbian" = "szerb", "Turkish" = "török",
  "Galego" = "galego", "Swedish" = "svéd", "Danish" = "dán",
  "Greek" = "görög", "Slovenian" = "szlovén", "Lithuanian" = "litván",
  "Macedonian" = "macedón", "Norwegian" = "norvég", "Icelandic" = "izlandi",
  "Chinese" = "kínai", "Vietnamese" = "vietnami", "Korean" = "koreai",
  "Russian" = "orosz", "Portuguese" = "portugál", "Estonian" = "észt",
  "Finnish" = "finn", "Slovakian" = "szlovák", "Albanian" = "albán",
  "Arabic" = "arab", "Brazilian Portuguese" = "brazil portugál",
  "Persian" = "perzsa", "Ukranian" = "ukrán", "Bengalian" = "bengáli")

# only for the English image
df <- df %>% 
  mutate(lang = names(lang.en)[match(df$lang, lang.en)])

langorder <- df %>% 
  filter(!is.na(lang)) %>% 
  select(lang, year) %>% 
  group_by(lang) %>% 
  summarise(y = min(year)) %>% 
  arrange(y) %>% 
  select(lang) %>% 
  unlist(use.names = FALSE)
langorder

ns <- df %>% 
  filter(!is.na(lang)) %>% 
  select(lang, year) %>%
  count(lang) %>% 
  mutate(lang = factor(lang, levels = rev(langorder))) %>% 
  arrange(desc(lang))
ns

df %>% 
  filter(!is.na(lang)) %>% 
  select(lang, year) %>% 
  mutate(i = 1) %>% 
  arrange(year) %>% 
  group_by(lang) %>% 
  mutate(j = cumsum(i)) %>%
  mutate(
    lang = factor(lang, levels = rev(langorder)),
  ) %>%
  ggplot(aes(y = lang, x = year)) +
  # geom_point(aes(size = j), shape = 'square') +
  # geom_line(aes(linewidth = j^2), ) +
  geom_segment(aes(xend = 2026, yend = lang, linewidth = log2(j) * 0.1), color='cornflowerblue') +
  geom_jitter(shape = 'square', width = 0.3, height = 0.3, alpha = 0.6) +
  geom_text(data = ns, 
            mapping = aes(x = 2027, label = sprintf("% 2s", n)), size = 2.7) +
  labs(
    title = NULL, # 'Krasznahorkai László fordításai',
    y = NULL, # 'nyelvek az első megjelenés sorrendjében',
    x = NULL, # 'a fordítás megjelenésének éve',
    j = 'fordítások\nszáma',
  ) +
  scale_linewidth(name = NULL, guide="none") + #'fordítások\nszáma') +
  scale_alpha(name = NULL) +
  scale_y_discrete(
    # sec.axis = dup_axis(),
  ) +
  theme_bw() +
  theme(
    
  )

ggsave('images/krasznahorkai/languages7.en.png',
       width = 5, height = 6, units = 'in', dpi = 300)

df %>% 
  filter(is.na(lang))
df %>% 
  group_by(lang) %>% 
  arrange(year) %>% 
  mutate(
    f = first(title),
    n = n()
  ) %>% 
  filter(title == f) %>% 
  select(lang, f, n) %>% 
  print(n = Inf)
# 