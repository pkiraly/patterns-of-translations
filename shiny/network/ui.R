library(shiny)
library(igraph)
library(DT)

navbarPage(
  "Patterns of Translations",
  tabPanel(
    "Network Analysis",
    fluidPage(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
      ),
      fluidRow(
        column(3,
          h4("Connections between regions"),
          radioButtons(
            "minmax",
            label = "criterium",
            choices = c(
              "Minimum number of authors" = "min",
              "Maximum number of authors" = "max"
            )),
          sliderInput("limit",
            label = "number of authors",
            min = 1,
            max = 100,
            value = 10,
            step = 1,
            width = '100%'),
          radioButtons(
            "layout",
            label = "chart type",
            choices = c(
              "network" = "network",
              "circle" = "layout_in_circle"
          )),
          selectInput(
            "country",
            "Select a region",
            choices = NULL,
          ),
          checkboxInput("level", "show 2 levels for a region"),
          p(
            id = "abbreviation-explanation",
            strong("ratio"),
            "shows how many times a region published authors before and after",
            "than others in the current network.",
            strong("result"),
            "is the summary of it. If this number is positive the region",
            "published more authors before than other regions."
          ),
          tableOutput("abbreviations")
        ),
        column(6,
          p("The network edges represent the number of authors translated in both regions. The direction represents the chronological order."),
          plotOutput(outputId = "network_plot", width = "100%", height = '600px'),
          h3("network metrics", class = "metrics"),
          tags$ul(
            class = "metrics",
            tags$li("diameter: the length of the longest shortest paths"),
            tags$li("degree: the number of its adjacent edges"),
            tags$li("betweenness: number of shortest paths going through a node"),
            tags$li("closeness: how many steps is required to access every other vertex from a given vertex")
          ),
          p(
            class = "metrics",
            "the diamater: ",
            textOutput("diameter", inline=TRUE)
          ),
          dataTableOutput("metrics"),
        ),
        column(3,
          dataTableOutput("data_table")
        )
      )
    ),
  ),
  tabPanel(
    "About",
    fluidPage(
      wellPanel(
        h4("Patterns of translations. Investigating the translated Hungarian literature"),
        p("Authors: András Kiséry (CCNY, New York) and Péter Király (GWDG, Göttingen)."),
        p("This is a set of interactive statistical dashboards for the analysis of 
          the Hungarian literature tranlated into foreign languages."),
        p("The dataset is based on",
          a("Bibliographica Hungaricana", href="http://demeter.oszk.hu", target="_blank"),
          "by Tibor Demeter and Tiborné Demeter, and UNESCO's",
          a("Index Translationum", href="https://www.unesco.org/xtrans/bsform.aspx", target="_blank"),
          "database."),
        p("The source code is available at",
          a("GiHhub", href="https://github.com/pkiraly/patterns-of-translations/", target="_blank"),
          "while the data is available at",
          a("GRO.data", href="https://doi.org/10.25625/5JFAMK", target="_blank"),
          "repository."
        ),
      )
    )
  ),
  collapsible = TRUE
)
