# Define UI function for Monty Hall module
monty_hall_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    plotOutput(outputId = ns("histogram"))
  )
}

# Define server function for Monty Hall module
monty_hall_server <- function(input, output, session) {
  output$histogram <- renderPlot({
    # Create a simple histogram
    data <- rnorm(100)
    hist(data, main = "Histogram for Monty Hall Problem")
  })
}
