library(shiny)

fluidPage(
  sliderInput("mean",
              label = "Mean",
              min = 1,
              max = 2,
              value = 1,
              step = 0.1),
  plotOutput("hist")
)
