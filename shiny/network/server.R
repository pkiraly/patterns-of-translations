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

edges_for_all <- function(common_authors_limit) {
  df %>% 
    filter(common_authors >= common_authors_limit) %>% 
    filter(first > 0) %>% 
    rename(weight = first) %>% 
    select(from, to, weight)
}

getNetwork <- function(common_authors_limit) {
  edges <- edges_for_all(common_authors_limit)
  nodes <- extractNodes(edges)
  graph_from_data_frame(d=edges, vertices=nodes, directed=TRUE)
}

edges_for_country <- function(country, common_authors_limit) {
  df %>% 
    filter(from == country | to == country) %>% 
    filter(common_authors >= common_authors_limit) %>% 
    filter(first > 0) %>% 
    rename(weight = first) %>% 
    select(from, to, weight)
}

getNetworkForCountry <- function(country, common_authors_limit) {
  edges <- edges_for_country(country, common_authors_limit)
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
  observeEvent(input$focus, {
    if (input$focus == "single") {
      updateSelectInput(
        inputId = "country",
        choices = setNames(regions$id, regions$region)
      )
    }
  })
  
  output$network_plot <- renderPlot({
    if (input$focus == "all") {
      net <- getNetwork(input$limit)
    } else {
      net <- getNetworkForCountry(input$country, input$limit)
    }
    E(net)$width <- (E(net)$weight / 30) ^ 1.5
    E(net)$label <- E(net)$weight
    V(net)$label.color <- c('blue', 'maroon', '#666666')[V(net)$world]
    par(mar = c(0, 0, 0, 0)) # set margin
    if (input$layout == "layout_in_circle") {
      plot(net, 
           # edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
           layout=layout_in_circle,
           vertex.label.color=c('blue', 'maroon', '#666666')[V(net)$world], 
           vertex.label.cex=1.5,
           vertex.shape="none", 
           edge.curved=.3,
           edge.label.cex=.7,
           edge.label.color="cornflowerblue",
           edge.arrow.size=.1, # E(net)$weight / 50,
      )
    } else {
      plot(net,
           # vertex.label.color="maroon", 
           vertex.label.cex=1.5,
           vertex.shape="none", 
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
    if (input$focus == "all") {
      edges <- edges_for_all(input$limit)
    } else {
      edges <- edges_for_country(input$country, input$limit)
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
    if (input$focus == "all") {
      edges <- edges_for_all(input$limit)
    } else {
      edges <- edges_for_country(input$country, input$limit)
    }
    inits <- edges %>% group_by(from) %>% summarise(init = sum(weight)) %>% rename(id = from)
    follows <- edges %>% group_by(to) %>% summarise(follow = sum(weight)) %>% rename(id = to)
    
    extractNodes(edges) %>% 
      mutate(world = as.character(world)) %>% 
      left_join(inits, join_by(id)) %>% 
      left_join(follows, join_by(id)) %>% 
      mutate(
        region = paste(region, " (", world, ")", sep = ""),
        ratio = paste(round(init), ":", round(follow), sep = ""),
        result = as.integer(round((init - follow))),
      ) %>% 
      select(-c(world, init, follow)) %>% 
      arrange(desc(result))
  })
}