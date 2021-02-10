library(tidyverse)
library(ggplot2)
library("stringdist")
source("scripts/functions.R")

df <- read_csv('data_raw/demeter.csv')

df %>% 
  select(nyelv) %>%
  group_by(nyelv) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  view()

df %>% 
  filter(grepl('Toinen ed', megjelenes)) %>% 
  select(megjelenes, nyelv) 
  #view()

cities <- read_csv('data_raw/normalized-cities.csv')
df %>% 
  filter(!grepl('^"', megjelenes)) %>% 
  filter(!grepl('^\\d', megjelenes)) %>% 
  mutate(megjelenes = gsub('Frankfurt am Main,1849', 'Frankfurt am Main, 1849', megjelenes)) %>% 
  mutate(megjelenes = gsub('Budapest,1961', 'Budapest, 1961', megjelenes)) %>% 
  mutate(megjelenes = gsub('Budapest,1880', 'Budapest, 1880', megjelenes)) %>% 
  mutate(megjelenes = gsub('Budapest,1938', 'Budapest, 1938', megjelenes)) %>% 
  mutate(megjelenes = gsub('Budapest,1970', 'Budapest, 1970', megjelenes)) %>% 
  mutate(megjelenes = gsub('Bern,1868', 'Bern, 1868', megjelenes)) %>% 
  mutate(megjelenes = gsub('Breslau, 905', 'Breslau, 1905', megjelenes)) %>% 
  mutate(megjelenes = gsub('Breslau, 1 905', 'Breslau, 1905', megjelenes)) %>% 
  mutate(megjelenes = gsub('Bucureşti, 954', 'Bucureşti, 1954', megjelenes)) %>% 
  mutate(megjelenes = gsub('Buffalo, 969', 'Buffalo, 1969', megjelenes)) %>% 
  mutate(megjelenes = gsub('Franckfurt, am Main,1849', 'Franckfurt, am Main, 1849', megjelenes)) %>% 
  mutate(megjelenes = gsub('Hermannstadt,1891', 'Hermannstadt, 1891', megjelenes)) %>% 
  mutate(megjelenes = gsub('Indianapolis, 926', 'Indianapolis, 1926', megjelenes)) %>% 
  mutate(megjelenes = gsub('^ilano', 'Milano', megjelenes)) %>% 
  mutate(megjelenes = gsub('Kasa, 1908', 'Kassa, 1908', megjelenes)) %>% 
  mutate(megjelenes = gsub('København, 1867 ', 'København, 1867.', megjelenes)) %>% 
  mutate(megjelenes = gsub('Konstanz am Bodensee, 196 ', 'Konstanz am Bodensee, 1916', megjelenes)) %>% 
  mutate(megjelenes = gsub('Kraków, 971', 'Kraków, 1971', megjelenes)) %>% 
  mutate(megjelenes = gsub('Leipzig, oJ.', 'Leipzig, o. J.', megjelenes)) %>% 
  mutate(megjelenes = gsub('Leipzig,1897', 'Leipzig, 1897', megjelenes)) %>% 
  mutate(megjelenes = gsub('Lochena, 1953', 'Lochem, 1953', megjelenes)) %>% 
  mutate(megjelenes = gsub('Liptivsky Sv. Mikuláš , 1944', 'Liptivsky Sv. Mikuláš, 1944', megjelenes)) %>% 
  mutate(megjelenes = gsub('London,1894', 'London, 1894', megjelenes)) %>% 
  mutate(megjelenes = gsub('Londonk, 1968', 'London, 1968', megjelenes)) %>% 
  mutate(megjelenes = gsub('Madris, 1969', 'Madrid, 1969', megjelenes)) %>% 
  mutate(megjelenes = gsub('Milano,1937', 'Milano, 1937', megjelenes)) %>% 
  mutate(megjelenes = gsub('Minden in Westfalien,1886', 'Minden in Westfalien, 1886', megjelenes)) %>% 
  mutate(megjelenes = gsub('Orades, 1926', 'Oradea, 1926', megjelenes)) %>% 
  mutate(megjelenes = gsub('Pest,1864', 'Pest, 1864', megjelenes)) %>% 
  mutate(megjelenes = gsub('Prag, O. J.', 'Prag, o. J.', megjelenes)) %>% 
  mutate(megjelenes = gsub('Prais, 1896', 'Paris, 1896', megjelenes)) %>% 
  mutate(megjelenes = gsub('Paris, 1930', 'Paris, 1930', megjelenes)) %>% 
  mutate(megjelenes = gsub('Rigā, 970', 'Rigā, 1970', megjelenes)) %>% 

  mutate(megjelenes = gsub('Datum unkbek\\.', 'Datum unbek.', megjelenes)) %>% 
  
  mutate(
    city = gsub(
      '^(.*?) \\(?(\\d\\d\\d(\\d|\\?)|o\\. ?[jJ]\\.?|s\\. ?a\\.?|b\\. ?r\\.?|Jahr unbek\\.|é\\. n\\.|s\\. ?d\\.|b\\. ?g\\.|Datum fehlt|Datum unbek\\.|Ohne Jahresang\\.|f\\. ?a\\.|w\\. ?y\\.|without year|bez\\. god\\.|sans date).*',
      '\\1', megjelenes),
    year = gsub(
      '^(.*?) \\(?(\\d\\d\\d(\\d|\\?)|o\\. ?[jJ]\\.?|s\\. ?a\\.?|b\\. ?r\\.?|Jahr unbek\\.|é\\. n\\.|s\\. ?d\\.|b\\. ?g\\.|Datum fehlt|Datum unbek\\.|Ohne Jahresang\\.|f\\. ?a\\.|w\\. ?y\\.|without year|bez\\. god\\.|sans date).*',
      '\\2', megjelenes)) %>% 

  mutate(year = clean_year(year)) %>% 
  mutate(city = clean_city(city)) %>% 
  mutate(city = gsub('Postsdam', 'Potsdam', city)) %>% 
  mutate(city = gsub('Potsdamm', 'Potsdam', city)) %>% 
  mutate(city = gsub('Postdam', 'Potsdam', city)) %>% 

  left_join(cities, by = c("city" = "source")) %>% 
  mutate(normalized_city = ifelse(is.na(normalized_city), city, normalized_city)) %>% 

  # group_by(normalized_city) %>% 
  # count() %>% 
  # arrange(normalized_city) %>%
  # view()
  
  group_by(normalized_city, year, nyelv) %>% 
  count() %>%
  filter(n > 1) %>% 
  arrange(normalized_city, year, nyelv) %>% 
  write_csv('data/city-year-language.csv')
  # view()

df %>% 
  filter(grepl('Josef Steinbach', fordito)) %>% 
  select(megjelenes) %>% 
  mutate(megjelenes = gsub(' \\(2\\. Auflage)$', '', megjelenes)) %>% 
  mutate(megjelenes = gsub(' 2. Auflage!!!$', '', megjelenes)) %>% 
  
  mutate(megjelenes = str_replace(megjelenes, 'Petöfis', 'Petőfis')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'P.-s Poetische', 'Petőfis Poetische')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Poetische Werke − Petöfi.', 'Petőfis Poetische Werke')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'poetische Werke', 'Poetische Werke')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Poetosche', 'Poetische')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Petöfiws', 'Petőfis')) %>% 
  
  mutate(megjelenes = str_replace(megjelenes, 'Verlags-Anstal\\.', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlags-Anstalgt', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Velags-Anst\\.', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlage-Anst\\.', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlags-Anst\\.', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlags Anstalt', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlag Anstalt', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlag-Anstalt', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlagen-Anstalt', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlags -Anstalt', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlags-An\\.', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlags-Ans\\.', 'Verlags-Anstalt')) %>% 
  # mutate(megjelenes = str_replace(megjelenes, 'Verlags-Anst', 'Verlags-Anstalt')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Verlag-Anstalt', 'Verlags-Anstalt')) %>% 
  
  mutate(megjelenes = str_replace(megjelenes, 'Schleische ', 'Schlesische ')) %>% 
  mutate(megjelenes = str_replace(megjelenes, 'Schlesisch ', 'Schlesische ')) %>% 
  mutate(megjelenes = gsub(' [sS][\\.,] ?\\d+\\.?$', '', megjelenes)) %>% 

  
  mutate(megjelenes = str_replace(megjelenes, '(Druck und )?Verlag von Josef Wajdits', 'Josef Wajdits')) %>% 

  mutate(megjelenes = (ifelse(
    stringsim(
      megjelenes,
      'Breslau, 1902. Schlesische Verlags-Anstalt "Petőfis Poetische Werke"'
    ) > 0.90,
    'Breslau, 1902. Schlesische Verlags-Anstalt "Petőfis Poetische Werke"',
    megjelenes
  ))) %>% 
  
  mutate(megjelenes = (ifelse(stringsim(megjelenes,
    'Gross-Kanizsa, 1882. Josef Wajdits "Steinbach: Heimatsklaenge"') > 0.90,
    'Gross-Kanizsa, 1882. Josef Wajdits "Steinbach: Heimatsklaenge"',
    megjelenes))) %>% 
  
  mutate(megjelenes = (ifelse(stringsim(megjelenes,
                                        'Breslau, 1905.') > 0.90,
                              'Breslau, 1905.',
                              megjelenes))) %>% 

  mutate(megjelenes = (ifelse(stringsim(megjelenes,
                                        'Breslau, 1905. 2. Auflage') > 0.90,
                              'Breslau, 1905.',
                              megjelenes))) %>% 

  mutate(megjelenes = (ifelse(stringsim(megjelenes,
    'Breslau, 1905. Schlesische Verlags-Anstalt "Petőfis Poetische Werke" (2. Ausgabe!)') > 0.90,
    'Breslau, 1905.',
    megjelenes))) %>% 
  
  group_by(megjelenes) %>% 
  count() %>%
  mutate(city = gsub('^(.*?) \\(?(\\d\\d\\d\\d).*', '\\1', megjelenes)) %>% 
  mutate(year = gsub('^(.*?) \\(?(\\d\\d\\d\\d).*', '\\2', megjelenes)) %>% 
  mutate(sim = stringsim(megjelenes, 'Breslau, 1905. 2. Auflage')) %>% 
  view()

df %>% 
  filter(nyelv == 'olasz') %>% 
  select(fordito) %>%

  # olasz
  mutate(fordito = gsub('^Traduzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Interpretazione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^A cura di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'originale ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione dall\'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotti da ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dagli originali ungheresi è dovuta a ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\' ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dal testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione per ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trascritto dal tedesco da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Scelti e tradotti da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Scelti i tradotti da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tarduzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. a cura di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. dal testo orig. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. dal testo orig. ungh. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. dall\'inglese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trduzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traudzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tratuzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzioni di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradizione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotta da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte dall\'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte integralemente de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto da ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dall\' originale è dovuta a  ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dagli originale ungherese è dovuta a ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dell\'ungherese e dovuta a ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dal teste orig. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dall testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dall\'originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dall\'ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduczione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradusione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradutione \\(sio) del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Raccolte e tradotte da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. interlineare di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dal testo orig. ungh. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradutto dal testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione autorizzata di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal testo orig. latino ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal testo orig. ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione e prefazione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione del ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Traduzione dal tedesco del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal tedesco di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall \'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'inglese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'originale ungherese in ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'originale ungheresi di ', '', fordito)) %>% 
  
  

  mutate(fordito = gsub('^Versione italiana di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione italiana del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione interlineare di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione in prosa con pref. e note di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione dall\'ungerese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione dall\' ungherese ', '', fordito)) %>% 
  mutate(fordito = gsub('^Unica traduzione italiana di ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Sanza traduttore$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza taduttore$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza tradottere$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza traduttare$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza traduttere$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza traduttore$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza Traduttore$', 'NA', fordito)) %>% 
  # filter(grepl("aus", fordito)) %>% 
  group_by(fordito) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  View()

df %>% 
  select(fordito) %>%
  # angol
  mutate(fordito = gsub('^Translated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^English by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Retold by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Magyar by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the german by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^\\(Translated) by ', '', fordito)) %>% 
  mutate(fordito = gsub('^\\(Translated by) ', '', fordito)) %>% 
  mutate(fordito = gsub('^With biography by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapted by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptation by ', '', fordito)) %>% 
  mutate(fordito = gsub('^From the original Hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tarnslated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated fron the Hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Hungarian into English by ', '', fordito)) %>% 
  mutate(fordito = gsub('^A translation by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated into english by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated in prose by ', '', fordito)) %>% 
  mutate(fordito = gsub('^by ', '', fordito)) %>% 
  mutate(fordito = gsub('^By ', '', fordito)) %>% 
  mutate(fordito = gsub('^Abridged from the Hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^English text by ', '', fordito)) %>% 
  mutate(fordito = gsub('^English text and introd. by ', '', fordito)) %>% 
  mutate(fordito = gsub('^English version by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Introduction and translations by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Rendered into English Verse by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tanslated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tranlslated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Transl. by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated and edited by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated and illustrated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated aut of the German by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the french by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the German by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the hungarina by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the orig. hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translater by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translation by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Hungarian with a foreword by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the original hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translations from the Hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Transletad by ', '', fordito)) %>% 
  mutate(fordito = gsub('^From the original hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Imitated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Edited and translated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^From the German by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Hungarian Authors by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Revorded by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Selected and translated from the Hungarian by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Selected and translated from the Hungary by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Acta Comparationis Litterarum Universarum by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Hungarian and with an Introduction by ', '', fordito)) %>% 
  mutate(fordito = gsub('^\\(Translated) By ', '', fordito)) %>% 
  mutate(fordito = gsub('^English and introduction by ', '', fordito)) %>% 
  mutate(fordito = gsub('^English text and introduction by ', '', fordito)) %>% 
  mutate(fordito = gsub('^English vers. by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Translated from the Hungarian By ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Without translator\'s name$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator\'s name omitted$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator isn\'t named$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without the name of the translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator\'s name is omitted$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translater unknown$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translators name omitted$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator unknow$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Written in english$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without ment. translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator ins\'t named$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator unkown$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Tarnslator unknown$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator unknown$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator unkinown$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator\'s name is not given$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator\'s name is not given$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Translator\'s name not mentioned$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without ment. of translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without name of the translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without name of translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without naming a translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without naming the translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without the translators name$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without translator$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without translator"s name$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Without translators name$', 'NA', fordito)) %>% 
  
  # német
  mutate(fordito = gsub('^Deutsch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt durch ', '', fordito)) %>% 
  mutate(fordito = gsub('^Metrisch übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Magyarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übertragen durch ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen...von ', '', fordito)) %>%
  mutate(fordito = gsub('^Aus dem Ungarischen... von ', '', fordito)) %>%
  mutate(fordito = gsub('^Ins Deutsche übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutscher Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ins Deutsche übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nachgedichtet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsche Fassung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übertr. durch ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verdeutschung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen durch ', '', fordito)) %>% 
  mutate(fordito = gsub('^Umdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ins Deutsche Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen...durch ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen..von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Frei übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ins Deutsche umgedichtet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nach dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nach den deutschen Fassung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nach der deutschen Fassung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nach der deutschen Fassungen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In metrischer Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetztung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Metrisch übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Lateinischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor. Übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In metr. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übers. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutscher Übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt und eingeleitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor. Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Versmass der Originale übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsch durchaus im Versmass des Originals von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Versmass des Originals übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In metrischer übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übertr. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berechtigte Übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen neu übers. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsche von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutscher nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In Deutscher Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ins Deutsche ungedichtet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berechtigte übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig berechtigte übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsche Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Original und deutscher Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutsche Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In die deutsche Sprache übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertr. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen metrisch übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen metrisch übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen Übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor. Übersetzung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsche Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen metrisch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übersetzht von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Auter. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor. übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ber. Übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berecht. übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsche Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig berechtigte Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Versmass der Originale Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In Deutsche übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutscher Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus de Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Englischen übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Englischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Russischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Unarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ung. übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem magyarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Unarischen.. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungar von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarische übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarische von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischem übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptiert von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen frei bearb. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen frei übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen frei Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen in Deutsche übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen metr. über. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ausgewaehlt und übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berecht. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berechtigte übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetz von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzer von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt und herausgegeben von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzungen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überstezt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überstzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überstzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verdeutscht von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verdeutschungen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verfasst von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Üvertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Unter Verwendung einer alten Übersetzung aus dem Ungarischen übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ungarische Lyrik Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Umdichtungen aus dem Ungarischen und ein Geleitwort von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Üersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übrsetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übetragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übetragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übesetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragung in die deutsche Sprache von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überzetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überträgung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragon von ', '', fordito)) %>% 
  mutate(fordito = gsub('^übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übetsetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragen und betreut von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragen und bearbeitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übertragen aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überstzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überssetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersezt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzung... und deutsche Bearbeitung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzung. Deutsch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzung neu bearbeitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetztz von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt und engel. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzt und bearbeitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersetst von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersets von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Überseetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übersatzt und eingeleitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Übergtragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Üb ersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Schrift übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Roman Übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Roman Deutsch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Novellen Deutsch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Novellen Autor. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu bearbeitete Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu bearbeitete übers. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu bearbeitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Neu bearb. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nachdichtungen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Detusch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutwch von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einz. Ber. übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig Autor. Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einz. Ber. übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig ber. Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig ber. Übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig berechtigte übertragung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig ermaechtigte Übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Frei nach dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Für die deutsche Bühne bearbeitet von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutschen Nachdichtung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In die deutsche Sprache übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ins deutsche Sprache übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Versmass des Originale übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Original und deutscher Nachdichtung herausgegeben von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen übertr. durch. ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autorisierte Übersetzung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Aus dem Ungarischen in eignen und fremden Übersetzungen herausgegeben von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autor Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Autorisierte Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Ber. Übersetzung aus dem Ung. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berecht. Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berechtigte Überesetzung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berechtigte Übersetzung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Berechtigte übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsch durchaus im Versm. des Orig. von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Deutsche übersetzung von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Die übertragung aus dem Ungarischen wurde besorgt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig Ber. übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig berecht. übertragung aus dem Ungarische von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig berecht. übertragungen aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig. berecht. Übertragung aus dem Ungarische von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Einzig. berecht. Übertragung aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Frei aus dem Ungarischen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Herausgegeben und übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Herausgegeben und übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Herausgegeben und Übertragen von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Herausgegeben und Übertragen von  ', '', fordito)) %>% 
  mutate(fordito = gsub('^Herausgegeben, übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Herausgg. und Übersetzt von ', '', fordito)) %>% 
  mutate(fordito = gsub('^Im Original und deutscher Nachdichtung Herausgegeben von ', '', fordito)) %>% 
  mutate(fordito = gsub('^In deutscher Übertragung herausgegeben von ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Ohne Angabe des Übersetzers$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzer unbekannt$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Übersetzer nicht angegeben$', 'NA', fordito)) %>% 
  
  # francia
  mutate(fordito = gsub('^Adaptation de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptation ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapté par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapt. par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapté du hongrois et préface par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptaté par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptatien de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptation: ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptations de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptazion de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapté du hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapté du Hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptée du hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptée du Hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptée par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptées par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adaptét par ', '', fordito)) %>% 
  mutate(fordito = gsub('^français de ', '', fordito)) %>% 
  mutate(fordito = gsub('^français par ', '', fordito)) %>% 
  mutate(fordito = gsub('^française de ', '', fordito)) %>% 
  mutate(fordito = gsub('^française de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Imité de Jokai par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Imité de M. Jokai par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Imité par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Interprétation de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Interprétation d\'', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduction a été in travail d\' équipe de ', '', fordito)) %>% 
  mutate(fordito = gsub('^La version française est de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Poèmes adaptés du hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Présentation et traduction ', '', fordito)) %>% 
  mutate(fordito = gsub('^Présentation par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Présenté par ', '', fordito)) %>% 
  mutate(fordito = gsub('^prose par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Publi en français par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Publié par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Quatre traductions de ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^provisoire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit en prose par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du Hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit sur l\'original par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction et adaptation de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction en vers par ', '', fordito)) %>% 
  mutate(fordito = gsub('^\\(Traduit par) ', '', fordito)) %>% 
  mutate(fordito = gsub('^Taduit par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tarnslated by ', '', fordito)) %>% 
  mutate(fordito = gsub('^Texte français de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Textes recueillis et commentés par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduciton de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur probablemant ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur probablement ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traducteurs et adaptateurs:  ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction an vers par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction d\' ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction d\'', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit \\(en prose) par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction du Hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction et commentaire de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction et Commentaire par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction française par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduction hongrois adaptée par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traductions de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traductions par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit \\(en vers) par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit \\(enprose) par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit de hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit de l\'allemand opar ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit de l\'allemand par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit de l\'anglais par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit de l\'esperanto par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit de l\'original hongrois par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du hongr. par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du hongrois \\(en prose) par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du hongrois en prose par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du hongrois et présenté par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit du Magyar par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit en collaboration avec ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit en prose Par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit en français par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduit en vers par ', '', fordito)) %>%
  mutate(fordito = gsub('^Transposition en vers français par ', '', fordito)) %>%
  mutate(fordito = gsub('^Version française de ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Sans indication du traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur inconnu$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sams indication du traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sand indication du traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sans indication du tradcuteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sans indication du Traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sans Indication du Traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sans indication in traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sans indication du traductuer$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Sans indications du traducteur$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur Incennu$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur inconau$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur non connu$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Traducteur non indique$', 'NA', fordito)) %>% 

  # olasz
  mutate(fordito = gsub('^Traduzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Interpretazione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^A cura di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'originale ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione dall\'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotti da ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dagli originali ungheresi è dovuta a ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\' ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dal testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione per ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trascritto dal tedesco da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Scelti e tradotti da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Scelti i tradotti da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tarduzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. a cura di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. dal testo orig. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. dal testo orig. ungh. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. dall\'inglese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trduzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traudzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tratuzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzzione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzioni di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradizione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotta da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte dall\'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotte integralemente de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto da ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dall\' originale è dovuta a  ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dagli originale ungherese è dovuta a ', '', fordito)) %>% 
  mutate(fordito = gsub('^La traduzione dell\'ungherese e dovuta a ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dal teste orig. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dall testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dall\'originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dall\'ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduczione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradusione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradutione \\(sio) del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Raccolte e tradotte da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Trad. interlineare di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradotto dal testo orig. ungh. da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradutto dal testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione autorizzata di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal testo orig. latino ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal testo orig. ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione e prefazione di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal tedesco del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dal tedesco di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall \'ungherese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall testo originale ungherese da ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'inglese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'originale ungherese in ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traduzione dall\'originale ungheresi di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione italiana di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione italiana del ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione interlineare di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione in prosa con pref. e note di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione dall\'ungerese di ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versione dall\' ungherese ', '', fordito)) %>% 
  mutate(fordito = gsub('^Unica traduzione italiana di ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Sanza traduttore$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza taduttore$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza tradottere$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza traduttare$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza traduttere$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza traduttore$', 'NA', fordito)) %>% 
  mutate(fordito = gsub('^Senza Traduttore$', 'NA', fordito)) %>% 
  
  
    # holland
  mutate(fordito = gsub('^Vertaling av', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaling door', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaling van', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaling ', '', fordito)) %>%
  mutate(fordito = gsub('^Vertaald door ', '', fordito)) %>%
  mutate(fordito = gsub('^Uit het hongaarsch vertaald door ', '', fordito)) %>%
  mutate(fordito = gsub('^Vertaald uit het Hongaarsch door ', '', fordito)) %>%
  mutate(fordito = gsub('^De Nederland. bewerking werd verzorgd door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaaling door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaald uit het hongaarsch door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het Hongaarsch vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaald uit het hongaarsch door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit Het hongaarsch vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaling: ', '', fordito)) %>% 
  mutate(fordito = gsub('^Geautor. vertaaling door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Geautoriseerde vertaaling uit het hengaarsch door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Met inleiding, uit het Hongaarsch vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Naar het Hongaarsch bewerkt en ingeleid door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Naar het Hongaarsch door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nederlandsch vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Nederlandse vertaling ', '', fordito)) %>% 
  mutate(fordito = gsub('^Overgeret door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Samengasteld en ingeleid door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Samenstelling en vertaling ', '', fordito)) %>% 
  mutate(fordito = gsub('^Geautoriseerde vertaaling van ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit hat Hongaarsch vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het hongaars vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het hongaarsch door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het Hongaarsch door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het hongaarsche vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het hongaarsch vertaakd door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Uit het hongaarschex vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaalddoor ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaaling uit het hongaars door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaaling uit het Hongaars door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaaling uit het hongaarsch van ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertalung uit het Hongaars door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaaling van ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertsal door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verttald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verttald ', '', fordito)) %>% 
  mutate(fordito = gsub('^Verzameld en verklaard door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vеrtaald door ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Preložil ', '', fordito)) %>% 
  mutate(fordito = gsub('^Přeložil ', '', fordito)) %>% 
  mutate(fordito = gsub('^Przełożyła ', '', fordito)) %>% 
  mutate(fordito = gsub('^Przełożył ', '', fordito)) %>% 
  mutate(fordito = gsub('^Přeložila ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Překlad ', '', fordito)) %>% 
  mutate(fordito = gsub('^Preklad ', '', fordito)) %>% 
  mutate(fordito = gsub('^V preklade ', '', fordito)) %>% 
  mutate(fordito = gsub('^Prebásnila ', '', fordito)) %>% 

  mutate(fordito = gsub('^Перевод ', '', fordito)) %>% 
  mutate(fordito = gsub('^Препев ', '', fordito)) %>% 
  mutate(fordito = gsub('^Преведе ', '', fordito)) %>% 
  mutate(fordito = gsub('^Перевел ', '', fordito)) %>% 
  mutate(fordito = gsub('^Превео ', '', fordito)) %>% 
  mutate(fordito = gsub('^с венгерского ', '', fordito)) %>% 
  mutate(fordito = gsub('^Пераклад ', '', fordito)) %>% 
  
  mutate(fordito = gsub('^Fordította ', '', fordito)) %>% 
  mutate(fordito = gsub('^Traducere de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Preveo ', '', fordito)) %>% 
  mutate(fordito = gsub('^Переклад ', '', fordito)) %>% 
  mutate(fordito = gsub('^Suomentanut ', '', fordito)) %>% 
  mutate(fordito = gsub('^În româneşte de ', '', fordito)) %>% 
  mutate(fordito = gsub('^În romîneşte de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Suomeksi runoillut ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradukis: ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradukis ', '', fordito)) %>% 
  mutate(fordito = gsub('^Tradusă de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Vertaald door ', '', fordito)) %>% 
  mutate(fordito = gsub('^Atdzejojis ', '', fordito)) %>% 
  mutate(fordito = gsub('^Pelovepolöl fa ', '', fordito)) %>% 
  mutate(fordito = gsub('^Adapté par ', '', fordito)) %>% 
  mutate(fordito = gsub('^Versión español de ', '', fordito)) %>% 
  mutate(fordito = gsub('^Øversat ved ', '', fordito)) %>% 
    
  group_by(fordito) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  write_csv('data/translators.csv')
  #view()
