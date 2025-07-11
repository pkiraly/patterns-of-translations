library(tidyverse)
library(igraph)
library(DT)

print('#### START')
dataDir <- 'data2025'

prepare_base_df <- function(df) {
  df2 <- df %>%
    distinct() %>%
    # filter(region1 != 'Hungary' & region2 != 'Hungary') %>%
    filter(!is.na(ratio)) %>% 
    # filter(common_authors > common_authors_limit) %>% 
    left_join(regions, join_by(region1 == region)) %>% 
    rename(from = id) %>% 
    left_join(regions, join_by(region2 == region)) %>% 
    rename(to = id) %>% 
    select(-c(region1, region2))
}

extractNodes <- function(df) {
  ids <- unique(c(df$from, df$to))
  regions %>% 
    filter(id %in% ids)
}

edges_for_all <- function(df, common_authors_limit, minmax) {
  if (minmax == "min") {
    df2 <- df %>% filter(common_authors >= common_authors_limit) 
  } else if (minmax == "max") {
    df2 <- df %>% filter(common_authors <= common_authors_limit) 
  }
  df2 %>% 
    filter(first > 0) %>% 
    rename(weight = first) %>% 
    filter(!is.na(from) & !is.na(to)) %>% 
    select(from, to, weight)
}

getNetwork <- function(df, common_authors_limit, minmax) {
  print('getNetwork')
  edges <- edges_for_all(df, common_authors_limit, minmax)
  print(edges, n = Inf)
  print('/edges_for_all')
  nodes <- extractNodes(edges)
  print(nodes)
  print('/extractNodes')
  graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
}

edges_for_country <- function(df, country, common_authors_limit, minmax, level = FALSE) {
  if (level == TRUE) {
    df2 <- df %>% 
      filter(from == country | to == country) %>% 
      filter(first > 0)
    
    if (minmax == "min") {
      df2 <- df2 %>% filter(common_authors >= common_authors_limit) 
    } else if (minmax == "max") {
      df2 <- df2 %>% filter(common_authors <= common_authors_limit) 
    }
    ids <- extractNodes(df2) %>% pull(id)

    if (length(ids[ids == country]) == 0) {
      df %>% filter(from == "NON EXISTENT") %>% 
        rename(weight = first)
    } else {
      df %>% 
        filter(from %in% ids | to %in% ids) %>% 
        filter(common_authors >= common_authors_limit) %>% 
        filter(first > 0) %>% 
        rename(weight = first) %>% 
        select(from, to, weight)
    }
  } else {
    df %>% 
      filter(from == country | to == country) %>% 
      filter(common_authors >= common_authors_limit) %>% 
      filter(first > 0) %>% 
      rename(weight = first) %>% 
      select(from, to, weight)
  }
}

getNetworkForCountry <- function(df, country, common_authors_limit, minmax, level) {
  edges <- edges_for_country(df, country, common_authors_limit, minmax, level)
  nodes <- extractNodes(edges)
  graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
}

regions <- read_csv(paste0(dataDir, '/regions.csv'), show_col_types = FALSE) %>% 
  arrange(id)

readWorldFile <- function(world) {
  file_name <- paste0(dataDir, '/ratios-by-regions-', world, '.csv')
  print(file_name)
  raw_df <- read_csv(file_name, show_col_types = FALSE)
  base_df <- prepare_base_df(raw_df)
  print('readWorldFile()')
  base_df
}

function(input, output, session) {
  ids <- c("all", regions$id)
  names <- c("all regions", regions$region)
  readData <- reactive({
    readWorldFile(input$world)
  })

  readAuthorRegionPairs <- reactive({
    file_name <- paste0(dataDir, '/author-region-pairs-', input$world, '.csv')
    print(file_name)
    pariDF <- read_csv(file_name, show_col_types = FALSE)
    print('readAuthorRegionPairs read')
    return(pariDF)
  })

  updateSelectInput(
    inputId = "country",
    choices = setNames(ids, names)
  )
  
  output$network_plot <- renderPlot({
    df <- readData()
    print(head(df))
    if (input$country == "all" || input$country == "") {
      print('country = all or empty')
      net <- getNetwork(df, input$limit, input$minmax)
      print('/getNetwork()')
      shapes <- rep('none', length(V(net)))
    } else {
      print('has a country')
      net <- getNetworkForCountry(df, input$country, input$limit, input$minmax, input$level)
      countries <- V(net)$name
      print(countries)
      shapes <- c('none', 'circle')[as.integer(countries == input$country) + 1]
    }
    ebn <- edge_betweenness(net)
    if (length(ebn) > 0) {
      edgeDf <- as_tibble(as_edgelist(net))
      names(edgeDf) <- c("from", "to")
      edgeDf$ebn <- ebn
    }

    # print(cliques(net))
    
    V(net)$shape <- shapes
    E(net)$width <- (E(net)$weight / 20) ^ 1.5
    E(net)$label <- E(net)$weight
    V(net)$label.color <- c('blue', 'maroon', 'darkgreen')[V(net)$world]
    par(mar = c(0, 0, 0, 0)) # set margin
    if (input$layout == "layout_in_circle") {
      plot(net, 
           # edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
           layout=layout_in_circle,
           vertex.label.cex=1.5,
           vertex.color=adjustcolor("white", alpha.f = .5),
           vertex.frame.color="#999999",
           edge.curved=.3,
           edge.label.cex=.7,
           edge.label.color="cornflowerblue",
           edge.arrow.size=.1, # E(net)$weight / 50,
      )
    } else {
      plot(net,
           # vertex.label.color="maroon", 
           vertex.label.cex=1.5,
           vertex.color=adjustcolor("white", alpha.f = .5),
           vertex.frame.color="#999999",
           edge.curved=.3,
           edge.label.cex=.7,
           edge.label.color="cornflowerblue",
           edge.arrow.size = .01 + (input$limit / 100), # E(net)$weight / 50,
      )
    }
    # edge.curved=.1
    # vertex.shape="none", vertex.label=nodes2$media
  }, width = 600, height = 600)
  
  output$network_edges <- renderDataTable({
    df <- readData()
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(df, input$limit, input$minmax)
    } else {
      edges <- edges_for_country(df, input$country, input$limit, input$minmax, input$level)
    }
    
    edges %>% 
      rename(score = weight) %>% 
      left_join(regions, join_by(from == id)) %>% 
      select(-from) %>% 
      mutate(from = paste(region, " (", world, ")", sep = "" )) %>% 
      select(-c(world, region)) %>% 
      left_join(regions, join_by(to == id)) %>% 
      select(-to) %>% 
      mutate(to = paste(region, " (", world, ")", sep = "" )) %>% 
      select(from, to, score) %>% 
      arrange(desc(score)) %>% 
      datatable(rownames = FALSE)
  })
  
  output$author_list <- renderText({
    author_region_pairs <- readAuthorRegionPairs()

    df <- readData()
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(df, input$limit, input$minmax)
    } else {
      edges <- edges_for_country(df, input$country, input$limit, input$minmax, input$level)
    }
    e <- edges %>% 
      rename(score = weight) %>% 
      left_join(regions, join_by(from == id)) %>% 
      left_join(regions, join_by(to == id)) %>% 
      select(region.x, region.y, score) %>% 
      rename(region1 = region.x, region2 = region.y)

    region_names <- author_region_pairs %>% 
      filter(region1 != region2) %>% 
      left_join(e, by = join_by(region1, region2)) %>% 
      filter(!is.na(score)) %>% 
      count(author) %>%
      arrange(desc(n), author) %>% 
      mutate(
        item = sprintf('%s (%d)', author, n)
      ) %>% 
      select(item) %>% 
      unlist(use.names = FALSE)
    
    paste(region_names, collapse = ', ')
  })

  output$abbreviations <- renderDataTable({
    df <- readData()
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(df, input$limit, input$minmax)
    } else {
      edges <- edges_for_country(df, input$country, input$limit, input$minmax, input$level)
    }
    inits <- edges %>% group_by(from) %>% summarise(init = sum(weight)) %>% rename(id = from)
    follows <- edges %>% group_by(to) %>% summarise(follow = sum(weight)) %>% rename(id = to)
    
    extractNodes(edges) %>% 
      mutate(world = as.character(world)) %>% 
      left_join(inits, join_by(id)) %>% 
      left_join(follows, join_by(id)) %>% 
      mutate(
        region = paste(region, " (", world, ")", sep = ""),
        before = ifelse(is.na(init), 0, init),
        after = ifelse(is.na(follow), 0, follow),
        sum = before + after,
        balance = before - after,
        # ratio = paste(round(before), ":", round(after), sep = ""),
        ratio = ifelse(after == 0, before, round(before / after, digits = 2)),
        score = round(log10(ratio * sum), digits = 2),
      ) %>% 
      select(-c(world, init, follow)) %>% 
      arrange(desc(score))
  })
  
  output$metrics <- renderDataTable({
    df <- readData()
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(df, input$limit, input$minmax)
    } else {
      edges <- edges_for_country(df, input$country, input$limit, input$minmax, input$level)
    }

    nodes <- extractNodes(edges)
    net <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
    
    degreeVector <- degree(net)
    nodeDf <- as_tibble(degreeVector)
    names(nodeDf) <- c("degree")
    nodeDf$id <- names(degreeVector)
    nodeDf <- nodeDf %>% 
      select(id, degree) %>% 
      mutate(
        page_rank = round(unname(page_rank(net)$vector) * 1000) / 1000,
        betweenness = round(unname(betweenness(net)) * 100) / 100,
        closeness_in = round(closeness(net, mode = "in") * 100000) / 100000,
        closeness_out = round(closeness(net, mode = "out") * 100000) / 100000,
        closeness_total = round(closeness(net, mode = "total") * 100000) / 100000,
      ) %>% 
      datatable(options = list(pageLength = 25))
  })
  
  output$diameter <- renderText({
    df <- readData()
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(df, input$limit, input$minmax)
    } else {
      edges <- edges_for_country(df, input$country, input$limit, input$minmax, input$level)
    }
 
    nodes <- extractNodes(edges)
    net <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
    
    diameter(net)
  })
}