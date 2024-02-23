function(input, output, session) {
  
  output$hist <- renderPlot(
    hist(rnorm(50, mean = input$mean), 
         xlim = c(-5, 5))
  )
  
}