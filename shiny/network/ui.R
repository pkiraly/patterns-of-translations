library(shiny)

navbarPage(
  "Patterns of Translations",
  tabPanel(
    "Network Analysis",
    fluidPage(
      h4("Connections between regions"),
      p("The network edges represent the number of authors translated in both regions."),
      sliderInput("limit",
              label = "Minimum nr of common authors",
              min = 5,
              max = 100,
              value = 10,
              step = 5),
      radioButtons("layout", label = "chart type", choices = c(
        "network" = "network",
        "circle" = "layout_in_circle"
      )),
      plotOutput(outputId = "network_plot", width = "100%")
    ),
  ),
  tabPanel(
    "About",
    fluidPage(
      wellPanel(
        h4("What is this?"),
        p("This is a set of interactive statistical dashboards for the analysis of 
          the Hungarian literature tranlated into foreign languages. The dataset
          is based on Bibliographica Hungaricana by Tibor Demeter and 
          Tiborné Demeter, and UNESCO's Index Translationum."),
        p("Authors: András Kiséry (CCNY, New York) and Péter Király (GWDG, Göttingen).")
      )
    )
  ),
  collapsible = TRUE
)
