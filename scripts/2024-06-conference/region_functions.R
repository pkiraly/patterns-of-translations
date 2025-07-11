calculate_pair <- function(ratios, country1, country2, selected_authors_by_region, base_stats) {
  if (country1 != country2) {
    au1 <- selected_authors_by_region %>% filter(region == country1)
    au2 <- selected_authors_by_region %>% filter(region == country2)
    common_authors <- intersect(au1$author, au2$author)
    intersect <- length(common_authors)
    if (intersect == 0) {
      first <- 0
      second <- 0
    } else {
      scores <- get_score_for_first_publication(common_authors, c(country1, country2))
      first <- scores %>% filter(region == country1) %>% select(score) %>% unlist(use.names = FALSE)
      second <- scores %>% filter(region == country2) %>% select(score )%>% unlist(use.names = FALSE)
    }
    regstat1 <- base_stats %>% filter(region == country1)
    regstat2 <- base_stats %>% filter(region == country2)
    ratios <- ratios %>% add_row(c1 = country1,
                                 c2 = country2,
                                 ratio = intersect / regstat1$authors,
                                 common_authors = intersect,
                                 first = first)
    ratios <- ratios %>% add_row(c1 = country2,
                                 c2 = country1,
                                 ratio = intersect / regstat2$authors,
                                 common_authors = intersect,
                                 first = second)
  } else {
    ratios <- ratios %>% add_row(c1 = country1,
                                 c2 = country2,
                                 ratio = NA,
                                 common_authors = NA,
                                 first = NA)
  }
  ratios
}

calculate_ratios <- function(current_world, all_authors, authors_by_region, base_stats) {
  if (current_world == 'all') {
    selected_authors_by_region <- authors_by_region
  } else {
    selected_authors <- all_authors %>% filter(world == current_world) %>% 
      select(author) %>% unlist(use.names = FALSE)
    selected_authors_by_region <- authors_by_region %>% 
      filter(author %in% selected_authors)
  }

  l <- length(base_stats$region)
  ratios <- as_tibble(data.frame(c1 = character(),
                                 c2 = character(),
                                 ratio = numeric(),
                                 common_authors = numeric(),
                                 first = numeric()))
  for (i in seq(l)) {
    country1 <- base_stats$region[i]
    for (j in seq(i, l)) {
      country2 <- base_stats$region[j]
      if (!is.na(country1) && !is.na(country2)) {
        ratios <- calculate_pair(ratios, 
                                 country1, country2, 
                                 selected_authors_by_region,
                                 base_stats)
      }
    }
  }
  ratios <- ratios %>% rename(region1 = c1, region2 = c2)
  ratios
}

calculate_ratios2 <- function(all_authors, positive, negative, authors_by_region, base_stats) {
  if (is.na(positive)) {
    selected_authors_by_region <- authors_by_region
  } else {
    selected_authors <- selectWorld(all_authors_df, row$positive, row$negative)
    selected_authors_by_region <- authors_by_region %>% 
      filter(author %in% selected_authors)
  }
  
  l <- length(base_stats$region)
  ratios <- as_tibble(data.frame(c1 = character(),
                                 c2 = character(),
                                 ratio = numeric(),
                                 common_authors = numeric(),
                                 first = numeric()))
  for (i in seq(l)) {
    country1 <- base_stats$region[i]
    if (!is.na(country1)) {
      for (j in seq(i, l)) {
        country2 <- base_stats$region[j]
        if (!is.na(country2)) {
          ratios <- calculate_pair(ratios, 
                                   country1, country2, 
                                   selected_authors_by_region,
                                   base_stats)
        }
      }
    }
  }
  ratios <- ratios %>% rename(region1 = c1, region2 = c2)
  ratios
}

calculate_author_region_pairs <- function(all_authors, positive, negative,
                                          authors_by_region, base_stats) {
  if (is.na(positive)) {
    selected_authors_by_region <- authors_by_region
  } else {
    selected_authors <- selectWorld(all_authors_df, row$positive, row$negative)
    selected_authors_by_region <- authors_by_region %>% 
      filter(author %in% selected_authors)
  }
  
  l <- length(base_stats$region)
  df <- as_tibble(data.frame(
    author = character(),
    c1 = character(),
    c2 = character()))
  for (i in seq(l)) {
    country1 <- base_stats$region[i]
    for (j in seq(i, l)) {
      country2 <- base_stats$region[j]
      au1 <- selected_authors_by_region %>% filter(region == country1)
      au2 <- selected_authors_by_region %>% filter(region == country2)
      common_authors <- intersect(au1$author, au2$author)
      intersect <- length(common_authors)
      if (intersect > 0) {
        for (author in common_authors) {
          df <- df %>% 
            add_row(
              author = author,
              c1 = country1,
              c2 = country2)
        }
      }
    }
  }
  df <- df %>% rename(region1 = c1, region2 = c2)
  df
}

get_score_for_first_publication <- function(authors, regions) {
  
  calulated_scores <- authors_by_region2 %>% 
    filter(author %in% authors & region %in% regions) %>% 
    group_by(author) %>%
    mutate(y = min(first_year)) %>% 
    filter(y == first_year) %>% 
    group_by(author) %>% 
    mutate(score = 1 / n()) %>% 
    group_by(region) %>% 
    summarise(score = sum(score), .groups = "drop")
  
  if (nrow(calulated_scores) == 2) {
    calulated_scores
  } else {
    base_scores <- tibble(region = regions, score = c(0, 0))
    base_scores %>% 
      left_join(calulated_scores, join_by(region)) %>% 
      mutate(score = ifelse(!is.na(score.y), score.y, score.x)) %>% 
      select(region, score)
  }
}

selectWorld <- function(authors_by_world, positive, negative = NA) {
  if (is.na(positive)) {
    selected <- authors_by_world
  } else {
    selected <- authors_by_world %>% 
      filter(grepl(positive, world))
  }
  
  if (!is.na(negative)) {
    selected <- selected %>% 
      filter(!grepl(negative, world))
  }
  selected %>% select(author) %>% unlist(use.names = FALSE)
}
