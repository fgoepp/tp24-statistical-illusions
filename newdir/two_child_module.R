# two_child_module.R
two_child_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Two Child Problem"),
    plotOutput(ns("histogram"))
  )
}

two_child_server <- function(input, output, session) {
  output$histogram <- renderPlot({
    hist(rnorm(100))
  })
}
