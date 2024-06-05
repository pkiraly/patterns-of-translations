calculate_ratios <- function(current_world, all_authors, authors_by_region, base_stats) {
  if (current_world == 'all') {
    selected_authors_by_region <- authors_by_region
  } else {
    selected_authors <- all_authors %>% filter(world == current_world) %>% 
      select(author) %>% unlist(use.names = FALSE)
    print(selected_authors)
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
      if (country1 != country2) {
        au1 <- selected_authors_by_region %>% filter(region == country1)
        au2 <- selected_authors_by_region %>% filter(region == country2)
        common_authors <- intersect(au1$author, au2$author)
        intersect <- length(common_authors)
        if (intersect == 0) {
          first <- 0
          second <- 0
        } else {
          scores <- getScoreForFirstPublication(common_authors, c(country1, country2))
          first <- scores[1]
          if (length(scores) > 1) {
            second <- scores[2]
          } else {
            second <- 0
          }
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
    }
  }
  ratios <- ratios %>% rename(region1 = c1, region2 = c2)
  ratios
}