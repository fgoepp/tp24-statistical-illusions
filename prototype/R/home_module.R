homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Welcome to the Shiny Apps Dashboard"),
    p("This project hosts multiple Shiny apps. Select an app from the sidebar.")
  )
}

homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # no server logic for home module
  })
}

