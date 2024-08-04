#' app4_module.R
#'
#' The St. Petersburg Paradox by David Marx-Stölting
#'
#' contains:
#' - Introduction to the St. Petersburg Paradox
#' - Basic Implementation of the Paradox
#' - Mathematical Background
#' - Historical Background
#' - Exploratory Implementation to play around
#' - References for further reading
#'
#' import:
#' - st-petersburg-intro.html
#' - st-petersburg-math.html
#' - st-petersburg-history.html
#' - st-petersburg-references.html
#'
#' export: /
#'
#' Last edited: 2024-07-23 by DMS

library(shiny)
library(shinydashboard)

# Define the UI part of the module
app4UI <- function(id) {
  ns <- NS(id)
  tagList(
    # Add title
    h1("St. Petersburg Paradox"),
    fluidRow(
      # Import introduction from html file
      column(
        width = 12,
        title = "Hello there",
        includeHTML("./data/st-petersburg_paradox/st-petersburg-intro.html")
      )
    ),
    fluidRow(
      # Add inputs and box for the basic game
      box(
        width = 12,
        title = "Main Setup: The Basic Game",
        status = "primary",
        solidHeader = TRUE,
        sliderInput(ns("basic_tosses"),
          "Number of coin tosses:",
          min = 1,
          max = 100,
          value = 40
        ),
        sliderInput(ns("basic_fee"),
          "Entry fee:",
          min = 4,
          max = 100,
          value = 20
        ),
        plotOutput(ns("basic_plot")),
        textOutput(ns("basic_win"))
      )
    ),
    fluidRow(
      # Import mathematical background from html file
      box(
        width = 6,
        title = "Mathematical Background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        includeHTML("./data/st-petersburg_paradox/st-petersburg-math.html")
      ),
      # Import historical background from html file
      box(
        width = 6,
        title = "Historical Background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        includeHTML("./data/st-petersburg_paradox/st-petersburg-history.html")
      ),
    ),
    fluidRow(
      # Add inputs and box for the exploratory variation
      box(
        width = 12,
        title = "Secondary Setup: Free Exploration",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        p("Hello again, below you are invited to explore the questions teased
        in the mathematical background section. Check out what happens when
        you change the throwing object, factor or the starting value, but 
        stay cautious since numbers might get so high R isn't able to handle
        them anymore nicely. Don't panic, just relax and enjoy the sensations
        of exponential maths"),
        sliderInput(ns("experimental_tosses"),
          "Number of tosses:",
          min = 1,
          max = 200,
          value = 40
        ),
        sliderInput(ns("experimental_fee"),
          "Entry fee:",
          min = 0,
          max = 200,
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
            "Eight-sided dice (D8)",
            "Ten-sided dice (D10)",
            "Twelve-sided dice (D12)",
            "Twenty-sided dice (D20)"
          )
        ),
        numericInput(ns("factor"),
          label = "Factor for multiplication of the starting prize each round",
          min = 1,
          max = 20,
          value = 2
        ),
        # Space for additional input features :D

        plotOutput(ns("experimental_plot")),
        textOutput(ns("experimental_win"))
      )
    ),
    fluidRow(
      # Import references from html file
      box(
        width = 12,
        title = "References",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        includeHTML("./data/st-petersburg_paradox/st-petersburg-references.html")
      )
    )
  )
}

# Define the server part of the module
app4Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({
      output$basic_plot <- renderPlot({
        par(mfrow = c(1, 2), mgp = c(1.7, 0.7, 0))

        # Generate tosses based on input$basic_tosses from UI
        n <- 1:input$basic_tosses

        # Generate the probability distribution for n coin tosses
        p <- 0.5^n

        # Calculate the amount of money one would receive after n coin tosses
        win_per_round <- 2^(n - 1) * 2

        # Draw the plot to visualize the two exponential functions for the
        # probability and total win starting with the latter
        plot(n, win_per_round / 1e+09,
          pch = 16,
          xlab = "",
          ylab = "",
          ylim = c(0, max(win_per_round) / 1e+09),
          col = "#000080",
          type = "h",
          lwd = 3,
          axes = FALSE
        )
        axis(4)
        mtext("Absolute win in the given round (in billion €)",
          side = 4,
          at = max(win_per_round) / 2e+09,
          line = 1.7
        )

        # Add the probabilities to the plot
        par(new = TRUE)
        plot(n, p * 100,
          col = "#008000",
          xlab = "Number of throws",
          ylab = "Probability to win in this round (in %)",
          ylim = c(0, 50),
          lwd = 3,
          type = "h"
        )

        legend("top", "center",
          legend = c("win probability", "possible win"),
          fill = c("#008000", "#000080")
        )

        # Calculate the expected value
        expected_value <- cumsum(p * win_per_round)

        # Update the slider input so that the fee stays inside the monetary
        # boundaries of the game
        updateSliderInput(
          session,
          "basic_fee",
          max = max(expected_value)
        )

        # Visualize the expected value
        plot(n, expected_value,
          xlab = "Number of throws",
          ylab = "Expected win (in €)",
          type = "l",
          col = "#34F558",
          lwd = 3,
        )

        # Add a line to visualize the entry fee
        abline(h = input$basic_fee, col = "#7C38F5", lwd = 3)

        # Calculate the intersection x-value between the fee and the expected
        # value
        intersect_index <- which(expected_value >= input$basic_fee)[1]

        # Visualize the intersection
        points(n[intersect_index],
          input$basic_fee,
          pch = 4,
          cex = 2,
          lwd = 3,
          col = "#F59A33"
        )

        legend("topleft",
          legend = c("expected value", "entry fee", "intersection"),
          fill = c("#34F558", "#7C38F5", "#F59A33")
        )
      })


      output$basic_win <- renderText({
        # Calculate values again as described above
        n <- 1:input$basic_tosses
        p <- 0.5^n
        win_per_round <- 2^(n - 1) * 2
        expected_value <- cumsum(p * win_per_round)
        entry_fee <- input$basic_fee

        winning_index <- which(win_per_round >= entry_fee)[1]
        intersect_index <- which(expected_value >= entry_fee)[1]

        # Add a small paragraph to explain the results visualized above
        paste0(
          "After ", n[winning_index], " coin tosses, you would already win
              at least the entry fee of ", entry_fee, "€ with a probability of ",
          round(p[winning_index], digits = 6) * 100, "%. \t",
          "On the long run, you can expect to win back your money
               after ", n[intersect_index], " tosses"
        )
      })

      output$experimental_plot <- renderPlot({
        # Add second plot introduction

        par(mfrow = c(1, 2), mgp = c(1.7, 0.7, 0))

        # Generate tosses based on input$experimental_tosses from UI
        n <- 1:input$experimental_tosses

        # Load the object that is being thrown
        object <- input$tossing_object

        # Generate the probability distribution for n coin tosses depending on
        # the chosen object
        sides <- switch(object,
          "Four-sided dice (D4)" = 4,
          "Six-sided dice (D6)" = 6,
          "Eight-sided dice (D8)" = 8,
          "Ten-sided dice (D10)" = 10,
          "Twelve-sided dice (D12)" = 12,
          "Twenty-sided dice (D20)" = 20,
          2
        ) # Default is the coin

        p <- (1 / sides)**n

        # Calculate the amount of money one would receive after n coin tosses
        # depending on the chosen factor and starting value
        win_per_round <- input$factor**(n - 1) * input$starting_value

        # Draw the plot to visualize the two exponential functions for the
        # probability and total win starting with the latter
        plot(n, win_per_round / 1e+09,
          pch = 16,
          xlab = "",
          ylab = "",
          ylim = c(0, max(win_per_round) / 1e+09),
          col = "#000080",
          type = "h",
          lwd = 3,
          axes = FALSE
        )
        axis(4)
        mtext("Absolute win in the given round (in billion €)",
          side = 4,
          at = max(win_per_round) / 2e+09,
          line = 1.7
        )

        # Add the probabilities to the plot
        par(new = TRUE)
        plot(n, p * 100,
          col = "#008000",
          xlab = "Number of throws",
          ylab = "Probability to win in this round (in %)",
          ylim = c(0, max(p) * 100),
          lwd = 3,
          type = "h"
        )

        legend("top", "center",
          legend = c("win probability", "possible win"),
          fill = c("#008000", "#000080")
        )

        # Calculate the expected value
        expected_value <- cumsum(p * win_per_round)

        # Update the slider input so that the fee stays inside the monetary
        # boundaries of the game
        updateSliderInput(
          session,
          "experimental_fee",
          min = win_per_round[2], # The game wouldn't make any sense if you bet
          # the minimal amount you are going to receive
          max = min(expected_value[input$experimental_tosses], 1e+12)
        )

        # Update the numeric input so that the factor doesn't lead to numbers
        # this way all the important sensations to be seen will still be seen
        updateNumericInput(
          session,
          "factor",
          min = switch(object,
            "Four-sided dice (D4)" = 1,
            "Six-sided dice (D6)" = 2,
            "Eight-sided dice (D8)" = 4,
            "Ten-sided dice (D10)" = 6,
            "Twelve-sided dice (D12)" = 8,
            "Twenty-sided dice (D20)" = 16,
            1
          ), # Default is the coin again
          max = switch(object,
            "Four-sided dice (D4)" = 8,
            "Six-sided dice (D6)" = 10,
            "Eight-sided dice (D8)" = 12,
            "Ten-sided dice (D10)" = 14,
            "Twelve-sided dice (D12)" = 16,
            "Twenty-sided dice (D20)" = 24,
            6
          )
        )

        # Visualize the expected value
        plot(n, expected_value,
          xlab = "Number of throws",
          ylab = "Expected win (in €)",
          type = "l",
          col = "#34F558",
          lwd = 3,
        )

        # Add a line to visualize the entry fee
        abline(h = input$experimental_fee, col = "#7C38F5", lwd = 3)

        # Calculate the intersection x-value between the fee and the expected
        # value
        intersect_index <- which(expected_value >= input$experimental_fee)[1]

        #  Visualize the intersection
        points(n[intersect_index], input$experimental_fee,
          pch = 4,
          cex = 2,
          lwd = 3,
          col = "#F59A33"
        )

        legend("topleft",
          legend = c("expected value", "entry fee", "intersection"),
          fill = c("#34F558", "#7C38F5", "#F59A33")
        )
      })

      output$experimental_win <- renderText({
        # Calculate values again as described above
        n <- 1:input$experimental_tosses

        object <- input$tossing_object

        sides <- switch(object,
                        "Four-sided dice (D4)" = 4,
                        "Six-sided dice (D6)" = 6,
                        "Eight-sided dice (D8)" = 8,
                        "Ten-sided dice (D10)" = 10,
                        "Twelve-sided dice (D12)" = 12,
                        "Twenty-sided dice (D20)" = 20,
                        2)  # Default is a coin
        
        # Generate the probability distribution for n tosses
        p <- (1 / sides)**n

        win_per_round <- input$factor**(n - 1) * input$starting_value

        expected_value <- cumsum(p * win_per_round)
        entry_fee <- input$experimental_fee


        winning_index <- which(win_per_round >= entry_fee)[1]
        intersect_index <- which(expected_value >= entry_fee)[1]

        # For some cases the probability to win gets extremely small. Hence
        # rounding to six digits would cause the result to state that a 0%
        # chance to win the game is possible. Those exceptions are taken care of
        win_probability <- if (p[winning_index] < 1e-06) {
          "below 0.0001"
        } else {
          round(p[winning_index], digits = 6) * 100
        }

        # For some cases the expected value will grow logarithmically causing
        # the intersection to be far too high. Hence NA values emerge.Those
        # exceptions are taken care of
        toss_number <- if(is.na(n[intersect_index]) && (input$factor == sides)){
          i <- 0
          while((i * expected_value[1]) != entry_fee){
            i <- i + 1
          }
          winning_toss <- i
          
          paste0(
            " expect to win back your money after ", winning_toss,
            " tosses"
          )
        } else if (is.na(n[intersect_index])) {
          "not expect to get your money back or the values got too big/ small 
          for R to handle :C"
        } else {
          paste0(
            " expect to win back your money after ", n[intersect_index],
            " tosses"
          )
        }

        winning_toss <- if(is.na(n[winning_index])){
          "Oh no, unfortunately the numbers got too high/ low D:"
        } else{
          paste0("After ", n[winning_index], " tosses, you would already win
              at least the entry fee of ", entry_fee, "€ with a probability of ",
          win_probability, "%. \t")
        }
        
        # Add a small paragraph to explain the results visualized above
        paste0(
          winning_toss,
          "On the long run, you can", toss_number
        )
      })
    })
  })
}
