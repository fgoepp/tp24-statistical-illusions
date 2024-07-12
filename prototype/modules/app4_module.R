library(shiny)
library(shinydashboard)

# Define the UI part of the module
app4UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        width = 12,
        title = "Introduction to St. Petersburg",
        status = "primary",
        solidHeader = TRUE,
        htmlOutput(ns("intro"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Basic Implementation",
        status = "primary",
        solidHeader = TRUE,
        sliderInput(ns("coin_tosses"),
          "Number of coin tosses:",
          min = 1,
          max = 100,
          value = 40
        ),
        sliderInput(ns("fee"),
          "Entry fee:",
          min = 1,
          max = 100,
          value = 20
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Basic Plot",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("basic_plot")),
        textOutput(ns("win"))
      )
    ),
    fluidRow(
      box(
        width = 6,
        title = "Historical Background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        htmlOutput(ns("history"))
      ),
      box(
        width = 6,
        title = "Mathematical Background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        htmlOutput(ns("maths"))
      ),
    ),
    fluidRow(
      box(
        width = 12,
        title = "Experimental Implementation",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput(ns("tosses"),
          "Number of tosses:",
          min = 1,
          max = 100,
          value = 40
        ),
        sliderInput(ns("entry_fee"),
          "Entry fee:",
          min = 1,
          max = 100,
          value = 20
        ),
        sliderInput(ns("starting_value"),
          "Starting prize",
          min = 1,
          max = 10,
          value = 2
        ),
        selectInput(ns("tossing_object"),
          label = "Object to be thrown",
          choices = c(
            "Coin",
            "Four-sided dice (D4)",
            "Six-sided dice (D6)",
            "Twenty-sided dice (D20)"
          )
        ),
        numericInput(ns("factor"),
          label = "Factor for multiplication of the starting prize each round",
          min = 1,
          max = 20,
          value = 2
        )
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Experimental Plot",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        plotOutput(ns("experimental_plot"))
      )
    ),
    fluidRow(
      box(
        width = 12,
        title = "Literature",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        htmlOutput(ns("literature"))
      )
    )
  )
}

# Define the server part of the module
app4Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$intro <- renderText({
      intro_text <- readLines("./texts/st-petersburg-intro.txt")
      intro_output <- paste(intro_text, collapse = "<br/>")
      HTML(intro_output)
    })
    
    observe({
      entry_fee <- input$fee
      tosses <- input$coin_tosses
      starting_value <- input$starting_value
      mult <- entry_fee * starting_value

      # updating the slider
      updateSliderInput(
        session,
        "tosses",
        min = ceiling(mult / 2),
        max = min(2 * mult, 200),
        value = tosses
      )

      output$basic_plot <- renderPlot({
        par(mfrow = c(1, 2), mgp = c(1.7, 0.7, 0))

        # Generate bins based on input$tosses from UI
        n <- 1:input$coin_tosses
        p <- 0.5^n

        win_per_round <- 2^(n - 1) * 2

        # Draw the plot with the specified number of tosses
        plot(n, p * 100,
          col = "forestgreen",
          xlab = "Number of throws",
          ylab = "Probability to win in this round (in %)",
          ylim = c(0, 50),
          type = "h"
        )
        mtext("Absolute win in the given round (in billion €)",
          side = 4,
          at = 25,
          line = 1.7
        )

        par(new = TRUE)
        plot(n, win_per_round / 1e+09,
          pch = 16,
          xlab = "",
          ylab = "",
          col = "darkblue",
          axes = FALSE
        )
        axis(4)
        expected_value <- cumsum(p * win_per_round)

        plot(n, expected_value,
          xlab = "Number of throws",
          ylab = "Expected win (in €)",
          type = "l"
        )
        abline(h = input$fee, col = "darkred")
      })

      output$win <- renderText({
        # Helper function to avoid that the function prints p = 0
        calculate_win_prob <- function(win_per_round, entry_fee, prob_dist) {
          if (mean(win_per_round > entry_fee) == 1) {
            prob_dist[1]
          } else {
            1 - sum(prob_dist[win_per_round <= entry_fee])
          }
        }

        n <- 1:input$tosses
        p <- 0.5^n
        win_per_round <- 2^(n - 1) * input$starting_value
        probability_to_win <- calculate_win_prob(win_per_round, input$fee, p)

        paste0(
          "Probability to win at least the entry fee of ",
          input$fee, "€ in the ", input$tosses, "-th toss: ",
          round(probability_to_win * 100, digits = 2), "%"
        )
      })

      output$maths <- renderText({
        math_text <- readLines("./texts/st-petersburg-math.txt")
        math_output <- paste0(math_text, collapse = "<br/>")
        HTML(math_output)
      })
      
      output$history <- renderText({
        history_text <- readLines("./texts/st-petersburg-history.txt")
        history_output <- paste(history_text, collapse = "<br/>")
        HTML(history_output)
      })
      
      output$experimental_plot <- renderPlot({
        par(mfrow = c(1, 2), mgp = c(1.7, 0.7, 0))

        # Generate bins based on input$tosses from UI
        n <- 1:input$tosses
        object <- input$tossing_object
        if (object == "Four-sided dice (D4)") {
          p <- 0.25^n
        } else if (object == "Six-sided dice (D6)") {
          p <- (1 / 6)^n
        } else if (object == "Twenty-sided dice (D20)") {
          p <- (1 / 20)^n
        } else {
          p <- 0.5^n
        }

        win_per_round <- input$factor^(n - 1) * input$starting_value

        # Draw the plot with the specified number of tosses
        plot(n, p * 100,
          col = "forestgreen",
          xlab = "Number of throws",
          ylab = "Probability to win in this round (in %)",
          ylim = c(0, max(p) * 100),
          type = "h"
        )
        mtext("Absolute win in the given round (in billion €)",
          side = 4,
          at = max(p) * 50,
          line = 1.7
        )

        par(new = TRUE)
        plot(n, win_per_round / 1e+09,
          pch = 16,
          xlab = "",
          ylab = "",
          col = "darkblue",
          axes = FALSE
        )
        axis(4)
        expected_value <- cumsum(p * win_per_round)

        plot(n, expected_value,
          xlab = "Number of throws",
          ylab = "Expected win (in €)",
          type = "l"
        )
        abline(h = input$entry_fee, col = "darkred")
      })
      
      output$literature <- renderText({
        literature <- readLines("./texts/st-petersburg-literature.txt")
        literature_output <- paste(literature, collapse = "<br/>")
        HTML(literature_output)
      })
    })
  })
}