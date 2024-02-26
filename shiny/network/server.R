library(tidyverse)
library(igraph)

getNetwork <- function(common_authors_limit) {
  regions <- read_csv('data/regions.csv')
  
  df <- read_csv('data/ratios-of-soviet-block-by-regions.csv')
  df <- df %>% 
    distinct() %>% 
    filter(region1 != 'Hungary' & region2 != 'Hungary') %>% 
    filter(!is.na(ratio)) %>% 
    filter(common_authors > common_authors_limit) %>% 
    left_join(regions, join_by(region1 == region)) %>% 
    rename(from = id) %>% 
    left_join(regions, join_by(region2 == region)) %>% 
    rename(to = id) %>% 
    select(-c(region1, region2))

  nodes <- df %>% 
    filter(common_authors > common_authors_limit) %>% 
    select(from) %>% 
    distinct() %>% 
    mutate(
      id = from,
      label = from
    )

  ids <- nodes %>% select(id) %>% unlist(use.names = FALSE)

  edges <- df %>% 
    filter(common_authors > common_authors_limit) %>% 
    filter(from %in% ids & to %in% ids) %>% 
    filter(row_number() %% 2 == 1) %>% 
    rename(weight = common_authors) %>% 
    select(from, to, weight)

  graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
} 

function(input, output, session) {
  output$network_plot <- renderPlot({
    net <- getNetwork(input$limit)
    E(net)$width <- E(net)$weight / 20
    E(net)$label <- E(net)$weight
    print(input$layout)
    par(mar = c(0, 0, 0, 0)) # set margin
    if (input$layout == "layout_in_circle") {
      plot(net, 
           # edge.color=c("dark red", "slategrey")[(E(net)$type=="hyperlink")+1],
           layout=layout_in_circle, edge.curved=.3,
           vertex.label.color="maroon", 
           vertex.label.cex=1.5,
           vertex.shape="none", 
           edge.label.color="cornflowerblue",
      )
    } else {
      plot(net,
           edge.curved=.3,
           vertex.label.color="maroon", 
           vertex.label.cex=1.5,
           vertex.shape="none", 
           edge.label.color="cornflowerblue",
      )
    }
    # edge.curved=.1
    # vertex.shape="none", vertex.label=nodes2$media
  
  }, width = 800, height = 600)
}