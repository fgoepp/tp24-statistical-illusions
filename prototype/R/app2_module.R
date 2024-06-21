app2UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Shiny App 2"),
    p("description of Shiny App 2"),
    fluidRow(
      box(width = 12, title = "App 2 Content", status = "primary", 
          solidHeader = TRUE,
          "content goes here"
      )
    )
  )
}

app2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # server logic 
  })
}
