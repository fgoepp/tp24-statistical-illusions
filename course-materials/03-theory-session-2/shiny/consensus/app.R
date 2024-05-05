library(shiny)
library(bslib)
library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

ui <- page_sidebar(
  title="consensusVis",
  sidebar = sidebar(
    helpText("Create demogrpahic maps with information from the 2010 U.S. Consensu"),
    
    selectInput(
      "var",
      label = "Choose a variable to display", 
      choices = list(
        "Percent White", 
        "Percent Black", 
        "Percent Asian",
        "Percent Hispanic"
      ),
      selected = "Percent White"
    ),
    
    sliderInput(
      "range",
      label = "Range of interest:",
      min = 0, max = 100, value = c(0,100)
    )
    
  ), 
  card(plotOutput("map"))
  
)

server <- function(input, output) {
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Percent White" = counties$white,
                   "Percent Black" = counties$black,
                   "Percent Hispanic" = counties$hispanic,
                   "Percent Asian" = counties$asian)
    
    color <- switch(input$var,
                    "Percent White" = "darkgreen",
                    "Percent Black" = "black",
                    "Percent Hispanic" = "darkorange",
                    "Percent Asian" = "darkviolet")
    
    legend <- switch(input$var,
                     "Percent White" = "% White",
                     "Percent Black" = "% Black",
                     "Percent Hispanic" = "% Hispanic",
                     "Percent Asian" = "% Asian")
    
    percent_map(data, color, legend, input$range[1], input$range[2])
  })
}


shinyApp(ui=ui, server=server)