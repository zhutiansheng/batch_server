library(ggplot2)
library(shiny)
library(plotly)
if (interactive()) {
  options(device.ask.default = FALSE)
  
  ui <- fluidPage(
    plotlyOutput("plot")
  )
  
  server <- function(input, output) {
    output$plot <- renderPlotly({
      df <- data.frame(dose=c("D0.5", "D1", "D2"),
                       len=c(4.2, 10, 29.5))
      p<-ggplot(data=df, aes(x=dose, y=len)) +
        geom_bar(stat="identity",width = 0.5)
      plotly_build(p)
    })
  }
  
  shinyApp(ui, server)
}