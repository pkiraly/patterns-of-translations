library(tidyverse)
library(igraph)
library(DT)

prepare_base_df <- function(df) {
  df %>%
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

edges_for_all <- function(common_authors_limit, minmax) {
  if (minmax == "min") {
    df2 <- df %>% filter(common_authors >= common_authors_limit) 
  } else if (minmax == "max") {
    df2 <- df %>% filter(common_authors <= common_authors_limit) 
  }
  df2 %>% 
    filter(first > 0) %>% 
    rename(weight = first) %>% 
    select(from, to, weight)
}

getNetwork <- function(common_authors_limit, minmax) {
  edges <- edges_for_all(common_authors_limit, minmax)
  nodes <- extractNodes(edges)
  graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
}

edges_for_country <- function(country, common_authors_limit, minmax, level = FALSE) {
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

getNetworkForCountry <- function(country, common_authors_limit, minmax, level) {
  edges <- edges_for_country(country, common_authors_limit, minmax, level)
  nodes <- extractNodes(edges)
  graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
}

regions <- read_csv('data/regions.csv',
                    show_col_types = FALSE) %>% 
  arrange(id)

raw_df <- read_csv('data/ratios-of-soviet-block-by-regions.csv',
                   show_col_types = FALSE)
df <- prepare_base_df(raw_df)

function(input, output, session) {
  ids <- c("all", regions$id)
  names <- c("all regions", regions$region)
  updateSelectInput(
    inputId = "country",
    choices = setNames(ids, names)
  )
  
  output$network_plot <- renderPlot({
    if (input$country == "all" || input$country == "") {
      net <- getNetwork(input$limit, input$minmax)
      shapes <- rep('none', length(V(net)))
    } else {
      net <- getNetworkForCountry(input$country, input$limit, input$minmax, input$level)
      countries <- V(net)$name
      shapes <- c('none', 'circle')[as.integer(countries == input$country) + 1]
    }
    ebn <- edge_betweenness(net)
    if (length(ebn) > 0) {
      print("edge_betweenness(net): ")
      print(str(ebn))
      edgeDf <- as.tibble(as_edgelist(net))
      names(edgeDf) <- c("from", "to")
      edgeDf$ebn <- ebn
    }

    # print(cliques(net))
    
    V(net)$shape <- shapes
    E(net)$width <- (E(net)$weight / 20) ^ 1.5
    E(net)$label <- E(net)$weight
    V(net)$label.color <- c('blue', 'maroon', '#666666')[V(net)$world]
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
  
  output$data_table <- renderDataTable({
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(input$limit, input$minmax)
    } else {
      edges <- edges_for_country(input$country, input$limit, input$minmax, input$level)
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

  output$abbreviations <- renderTable({
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(input$limit, input$minmax)
    } else {
      edges <- edges_for_country(input$country, input$limit, input$minmax, input$level)
    }
    inits <- edges %>% group_by(from) %>% summarise(init = sum(weight)) %>% rename(id = from)
    follows <- edges %>% group_by(to) %>% summarise(follow = sum(weight)) %>% rename(id = to)
    
    extractNodes(edges) %>% 
      mutate(world = as.character(world)) %>% 
      left_join(inits, join_by(id)) %>% 
      left_join(follows, join_by(id)) %>% 
      mutate(
        init = ifelse(is.na(init), 0, init),
        follow = ifelse(is.na(follow), 0, follow),
        region = paste(region, " (", world, ")", sep = ""),
        ratio = paste(round(init), ":", round(follow), sep = ""),
        result = as.integer(round((init - follow))),
      ) %>% 
      select(-c(world, init, follow)) %>% 
      arrange(desc(result))
  })
  
  output$metrics <- renderDataTable({
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(input$limit, input$minmax)
    } else {
      edges <- edges_for_country(input$country, input$limit, input$minmax, input$level)
    }

    nodes <- extractNodes(edges)
    net <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
    
    degreeVector <- degree(net)
    nodeDf <- as.tibble(degreeVector)
    names(nodeDf) <- c("degree")
    nodeDf$id <- names(degreeVector)
    nodeDf <- nodeDf %>% 
      select(id, degree) %>% 
      mutate(
        page_rank = round(unname(page_rank(net)$vector) * 1000) / 1000,
        betweenness = unname(betweenness(net)),
        closeness_in = round(closeness(net, mode = "in") * 100000) / 100000,
        closeness_out = round(closeness(net, mode = "out") * 100000) / 100000,
        closeness_total = round(closeness(net, mode = "total") * 100000) / 100000,
      ) %>% 
      datatable()
  })
  
  output$diameter <- renderText({
    if (input$country == "all" || input$country == "") {
      edges <- edges_for_all(input$limit, input$minmax)
    } else {
      edges <- edges_for_country(input$country, input$limit, input$minmax, input$level)
    }
 
    nodes <- extractNodes(edges)
    net <- graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
    
    diameter(net)
  })
}