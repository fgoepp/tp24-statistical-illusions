library(shiny)
library(bslib)
library(plotly)

app3UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Shiny App 3"),
    p("Description of Shiny App 3"),
    fluidRow(
      box(width = 12, title = "App 3 Content", status = "primary", 
          solidHeader = TRUE,
          layout_sidebar(
            sidebar = sidebar("Dolor porttitor dapibus sed leo luctus, sed placerat? Tortor consequat hendrerit, 
                        duis venenatis odio, neque inceptos pretium? Vestibulum egestas netus fringilla facilisi dictum justo, 
                        iaculis faucibus suscipit: dictum conubia nulla. Mi natoque porta lacus congue lectus curae laoreet risus feugiat."),
            sidebar_width = 3,
            fluidRow(
              column(12,
                     "Sie reden mit jemandem, den sie nie früher getroffen haben.
          Er sagt ihnen, dass ",
                     tags$span("er zwei Kinder hat"
                               , style = "color: blue; font-weight: bold;"),
                     " Zusätzlich sagt er:"
              ),
              column(12, selectInput(
                inputId = ns("dropdown_menu"),
                label = NULL,
                choices = c("(zumindest) eines der", "das älteste", "Problem 3"),
                selected = "(zumindest) eines der"
              )),
              column(12, " Wie hoch ist in diesem Fall die Wahrscheinlichkeit, dass beide Kinder Jungen sind?"),
              conditionalPanel(
                condition = sprintf("input['%s'] == '(zumindest) eines der'", ns("dropdown_menu")),
                fluidRow(
                  column(3,
                         img(src = "BB.png", width = "100%"),
                         tags$p("")
                  ),
                  column(3,
                         img(src = "GG-crossed.png", width = "75%"),
                         tags$p("")
                  ),
                  column(3,
                         img(src = "Bg.png", width = "100%"),
                         tags$p("")
                  ),
                  column(3,
                         img(src = "Gb.png", width = "65%"),
                         tags$p("")
                  )
                )
              ),
              conditionalPanel(
                condition = sprintf("input['%s'] == 'das älteste'", ns("dropdown_menu")),
                fluidRow(
                  column(3,
                         img(src = "BB.png", width = "100%"),
                         tags$p("")
                  ),
                  column(3,
                         img(src = "GG-crossed.png", width = "75%"),
                         tags$p("")
                  ),
                  column(3,
                         img(src = "Bg.png", width = "100%"),
                         tags$p("")
                  ),
                  column(3,
                         img(src = "Gb-crossed.png", width = "65%"),
                         tags$p("")
                  )
                )
              ),
              fluidRow(
                column(12,
                       actionButton(inputId = ns("show_answer"), label = "Show Answer"),
                       textInput(inputId = ns("guess_probability"), label = "Guess the Probability", placeholder = "Enter a number"),
                       actionButton(inputId = ns("submit_guess"), label = "Submit")
                )
              )
            )
          )
      )
    ),
    fluidRow(
      box(width = 12, title = "App 3 Content", status = "primary", 
          solidHeader = TRUE,
          fluidRow(
            column(12,
                   sliderInput(ns("n_children"), "Number of children:", min = 2, max = 10, value = 2)
            ),
            column(6, plotlyOutput(ns("bar_plot"))),
            column(6, plotlyOutput(ns("line_plot")))
          )
      )
    )
  )
}

app3Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$dropdown_menu, {
      option <- input$dropdown_menu
      if (!is.null(option) && !is.na(option)) {
        if (option == "(zumindest) eines der") {
          print("Option 1 was selected.")
          # Perform action for option 1
        } else if (option == "das älteste") {
          print("Option 2 was selected.")
          # Perform action for option 2
        } else if (option == "Problem 3") {
          print("Option 3 was selected.")
          # Perform action for option 3
        }
      }
    })
    
    observeEvent(input$show_answer, {
      # Display the answer based on the selected option
      option <- input$dropdown_menu
      if (!is.null(option) && !is.na(option)) {
        if (option == "(zumindest) eines der") {
          showModal(modalDialog(
            title = "Answer",
            "1/3 => warum?? text..."
          ))
        } else if (option == "das älteste") {
          showModal(modalDialog(
            title = "Answer",
            "1/2 => warum?? text..."
          ))
        }
        # Add more conditions in the future
      }
    })
    
    observeEvent(input$submit_guess, {
      # Check if the guessed probability matches the answer
      option <- input$dropdown_menu
      answer_one <- 1 / 3
      answer_two <- 1 / 2  
      guess <- as.numeric(input$guess_probability)
      if (!is.na(guess)) {
        if ((guess == answer_one && option == "(zumindest) eines der") 
            || (guess == answer_two && option == "das älteste")) {
          showModal(modalDialog(
            title = "Result",
            "Yes, correct!"
          ))
        } else {
          showModal(modalDialog(
            title = "Result",
            "No, try again!"
          ))
        }
      }
    })
    
    plot_data <- reactive({
      n <- input$n_children
      data.frame(
        Case = c(paste(n, "children"),
                 paste(n, "children, at least one is a boy"),
                 paste(n, "children, the oldest is a boy")),
        Probability = c(1 / (2^n), 1 - (1 / (2^n)), 1 / 2)
      )
    })
    
    output$bar_plot <- renderPlotly({
      data <- plot_data()
      plot_ly(data, x = ~Case, y = ~Probability, type = 'bar', text = ~paste("Probability:", Probability),
              hoverinfo = 'text') %>%
        layout(title = "Bar Plot of Probabilities",
               xaxis = list(title = "Obscurity of information"),
               yaxis = list(title = "Probability")) %>%
        config(displayModeBar = FALSE)
    })
    
    output$line_plot <- renderPlotly({
      data <- plot_data()
      plot_ly(data, x = ~Case, y = ~Probability, type = 'scatter', mode = 'lines+markers', text = ~paste("Probability:", Probability),
              hoverinfo = 'text') %>%
        layout(title = "Line Plot of Probabilities",
               xaxis = list(title = "Obscurity of information"),
               yaxis = list(title = "Probability")) %>%
        config(displayModeBar = FALSE)
    })
  })
}