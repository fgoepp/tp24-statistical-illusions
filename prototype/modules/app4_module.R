library(shiny)
library(shinydashboard)

# Define the UI part of the module
app4UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("St. Petersburg Paradox"),
    p("Welcome traveller, its a freezing night in St. Petersburg and after a long 
      day you pike into a dive bar. You order a drink while all of a sudden the 
      only other person in the room starts talking and suggests a game. Even though
      you're not a gambler you consider to play. What else would you do with your 
      evening anyways? The game goes as follows: You pay a set entry fee to start
      the game. Afterwards a coin is flipped until heads comes up, as soon as that
      happens, the game is over and you win a price. Starting at 2€ if the coin 
      lands heads up directly in the first round, with every tails flip the starting
      value doubles (so winning on the 3rd toss would mean a return of 8€). The 
      questions now are: How much money should you bet at max. to be expected to 
      win more money then you paid as an entry fee? Would you bet more then 20€?
      More then 100€? \n \n
      You may see what happens to the expected value, win probability and possible 
      net win on the plots down below. Feel free to play around a little with 
      parameters such as the entry fee, starting value or number of total throws
      by using the given sliders. Below you will also find some historical and 
      mathematical background information for this statistical Paradox. \n
      Have fun :D"),
    fluidRow(
      box(
        width = 12, title = "Basic Sliders", status = "primary", solidHeader = TRUE,
        sliderInput(ns("tosses"), "Number of coin tosses:", min = 1, max = 100, value = 10),
        sliderInput(ns("fee"), "Entry fee:", min = 1, max = 100, value = 20),
        sliderInput(ns("starting_value"), "How much money should we start with?", min = 1, max = 10, value = 2)
      )
    ),
    fluidRow(
      box(
        width = 12, title = "Basic Plot", status = "primary", solidHeader = TRUE,
        plotOutput(ns("plot")),
        textOutput(ns("win"))
      )
    )
  )
}

# Define the server part of the module
app4Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    output$plot <- renderPlot({
      par(mfrow = c(1, 2))

      # Generate bins based on input$tosses from UI
      n <- 1:input$tosses
      p <- 0.5^n

      # Draw the plot with the specified number of tosses
      plot(p ~ n,
        col = "forestgreen", border = "blue",
        xlab = "Number of throws",
        ylab = "Probability to win in this round",
        type = "h"
      )

      win_per_round <- 2^(n - 1) * input$starting_value

      expected_value <- cumsum(p * win_per_round)

      plot(expected_value,
        xlab = "Number of throws",
        ylab = "Expected value",
        type = "l"
      )
      abline(h = input$fee, col = "darkred")
    })

    output$win <- renderText({
      # Helper function
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
        "Probability to win more than the entry fee of ", input$fee, "€: ",
        round(probability_to_win * 100, digits = 2), "%"
      )
    })
  })
}



#' TODO: 
#' - Add additional plot + sliders for experimental possibilities
#' - Visualize the win per round in the same plot as the probabilities
