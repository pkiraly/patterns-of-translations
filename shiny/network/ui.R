library(shiny)

navbarPage(
  "Patterns of Translations",
  tabPanel(
    "Network Analysis",
    fluidPage(
      fluidRow(
        column(3,
          h4("Connections between regions"),
          p("The network edges represent the number of authors translated in both regions."),
          sliderInput("limit",
              label = "Minimum number of authors",
              min = 5,
              max = 100,
              value = 10,
              step = 5),
          radioButtons("layout", label = "chart type", choices = c(
            "network" = "network",
            "circle" = "layout_in_circle"
          )),
        ),
        column(9,
           plotOutput(outputId = "network_plot", width = "100%")
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
