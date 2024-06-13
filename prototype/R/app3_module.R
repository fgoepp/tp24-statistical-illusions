app3UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Shiny App 3"),
    p("description of Shiny App 3"),
    fluidRow(
      box(width = 12, title = "App 3 Content", status = "primary", 
          solidHeader = TRUE,
          "content goes here."
      )
    )
  )
}

app3Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # server logic
  })
}
