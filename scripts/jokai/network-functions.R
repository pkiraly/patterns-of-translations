# helper functions to calculate and draw network

translate_languages <- function(.df) {
  .df %>% 
    mutate(
      lang = case_match(
        lang,
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
      lang = ifelse(between(year, 1945, 1989)
                    & lang == 'German' 
                    & !is.na(country) & country == 'GDR', 
                    paste0('DDR\n', lang), lang),
      lang = ifelse(lang == 'German' 
                    & !is.na(country) 
                    & country == 'Hungary',
                    paste0('HU\n', lang), lang),
      lang = ifelse(
        lang == 'English',
        ifelse(!is.na(country) & country == 'UK', paste0('UK\n', lang),
        ifelse(!is.na(country) & country == 'USA', paste0('US\n', lang),
        ifelse(!is.na(country) & country == 'Hungary', paste0('HU\n', lang),
        lang))),
        lang
      )
    )
}

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

#' Filter data frame by weight
#' 
#' @param .df The data frame
#' @param .limit The limit of rows to enter
#' @param show_all Whether or not display all edges between nodes or only the top ones (default: false)
#' @return The filtered dataframe
filter_by_weight <- function(.df, .limit, show_all = FALSE) {
  print(sprintf('filter_by_weight) .limit=%d', .limit))
  
  # edges <- .df  %>% 
  #  filter(weight >= .limit)
  #  head(.limit)
  
  if (.limit == FALSE) {
    edges <- .df
  } else {
    .weight <- .df %>% slice_head(n = .limit) %>% 
      slice_tail(n = 1) %>% pull(weight)
    
    edges <- .df  %>% 
      filter(weight >= .weight)
  }
    
  # print(edges)
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

draw_plot <- function(.df, .limit = 30, show_all = FALSE) {
  print(sprintf('draw_plot) .limit=%d, show_all=%s', .limit, show_all))
  edges <- filter_by_weight(.df, .limit, show_all)
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

