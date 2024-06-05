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
      ratios <- calculate_pair(ratios, 
                               country1, country2, 
                               selected_authors_by_region,
                               base_stats)
    }
  }
  ratios <- ratios %>% rename(region1 = c1, region2 = c2)
  ratios
}

get_score_for_first_publication <- function(authors, regions) {
  base_scores <- data.frame(region = regions, score = c(0, 0))
  
  calulated_scores <- authors_by_region2 %>% 
    filter(author %in% authors & region %in% regions) %>% 
    group_by(author) %>%
    mutate(y = min(first_year)) %>% 
    filter(y == first_year) %>% 
    group_by(author) %>% 
    mutate(score = 1 / n()) %>% 
    group_by(region) %>% 
    summarise(score = sum(score), .groups = "drop")
  
  base_scores %>% 
    left_join(calulated_scores, join_by(region)) %>% 
    mutate(score = ifelse(!is.na(score.y), score.y, score.x)) %>% 
    select(region, score)
}
