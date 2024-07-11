# Define the UI for the Monty Hall module
monty_hall_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Monty Hall Problem"),
    plotOutput(ns("histogram"))
  )
}

# Define the server logic for the Monty Hall module
monty_hall_server <- function(input, output, session) {
  # Generate a simple histogram as an example
  output$histogram <- renderPlot({
    hist(runif(100))
  })
}
