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
            "world",
            label = "based on authors published",
            choices = c(
              "only in 1st world" = "1",
              "only in 2nd world" = "2",
              "in 1st and 2nd world" = "1-2",
              "everywhere" = "all"
          )),
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
        ),
        column(9,
          fluidRow(
            column(9,
              p("The network edges represent the number of authors translated in both regions. The direction represents the chronological order."),
              plotOutput(outputId = "network_plot", width = "100%", height = '600px'),
            ),
            column(3,
              dataTableOutput("data_table")
            ),
          ),
          fluidRow(
            column(12,
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
              h3("region statistics", class = "metrics"),
              p(
                id = "abbreviation-explanation",
                tags$ul(
                  class = "metrics",
                  tags$li(strong("before"),
                    "is the number of times there was an author published in this region before another region in the current network (all definitions apply to the current network)"),
                  tags$li(strong("after"),
                    "is the number of times there was an author published in this region after another region"),
                  tags$li(strong("sum"),
                    "is the total number of times there was an author published before or after another region (sum=before+after)"),
                  tags$li(strong("balance"),
                    "is the difference of 'before' and 'after' values"),
                  tags$li(strong("ratio"),
                    "is the quotient of 'before' and 'after' values"),
                  tags$li(strong("score"),
                    "is the logarithmic value of 'ratio' multiplied by 'sum': log",
                    tags$sub('10'),
                    "(('before'+'after')*('before'/'after'))")
                )
              ),
              dataTableOutput("abbreviations")
            )
          )
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
