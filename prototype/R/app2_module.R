library(shiny)
library(styler)

app2UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    h2("Monty Hall Problem"),
    fluidRow(
      # introduction
      box(
        title = "Introduction",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        collapsed = FALSE,
        width = 12,
        p("The Monty Hall problem is a well known probability puzzle based on a
        game show scenario. A contestant is presented with three doors, behind one of
        which is a car (the prize) and behind the other two, goats (blank). After the
        contestant picks a door, the host (Monty Hall), who knows what's behind all
        the doors, opens one of the remaining doors to reveal a goat. The contestant
        is then given the option to stick with their original choice or switch to
        the other unopened door. Surprisingly, switching doors increases the
        contestant's chances of winning the car from 1/3 to 2/3.")
      ),
      # first "diagram"
      box(
        title = "Plot 1",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        sliderInput(ns("num_doors"),
                    "Number of doors:",
                    min = 3,
                    max = 20,
                    value = 3,
                    ticks = TRUE
        ),
        uiOutput(ns("door_selection")),
        actionButton(ns("run_single_simulation"), "Run single Simulation"),
        actionButton(ns("run_multiple_simulations"), "Run 100 Simulations"),
        verbatimTextOutput(ns("result"))
      ),
      # second "diagram"
      box(
        title = "Plot 2",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        sliderInput(ns("min_num_doors"),
                    "Minimum Number of doors:",
                    min = 3,
                    max = 100,
                    value = 3,
                    step = 1,
                    ticks = TRUE
        ),
        sliderInput(ns("num_car_doors"),
                    "Number of car doors:",
                    min = 1,
                    max = 2,
                    value = 1,
                    step = 1,
                    ticks = TRUE
        ),
        plotOutput(ns("win_plot"))
      ),
      # Historical Background
      box(
        title = "Historical Background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        p("The Monty Hall problem gained widespread attention in 1990 when
          Marilyn vos Savant, a columnist for Parade magazine, addressed it in
          her column Ask Marilyn. Her solution, which recommended always
          switching doors, sparked controversy and debate among readers,
          including many mathematicians and statisticians. Despite initial
          skepticism, vos Savant's explanation was correct and is now a
          well-known example used to illustrate the counterintuitive nature of
          probability and conditional probability.")
      ),
      # Mathematical Background
      box(
        title = "Mathematical Background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        p("")
      ),
      # References
      box(
        title = "References",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        p("")
      )
    )
  )
}

app2Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$door_selection <- renderUI({
      radioButtons(ns("initial_choice"), "Choose a door:",
                   choices = setNames(
                     seq_len(input$num_doors),
                     paste("Door", seq_len(input$num_doors))
                   )
      )
    })
    
    run_simulation <- function(num_doors, chosen_door) {
      car_door <- sample(seq_len(num_doors), 1)
      free_doors <- setdiff(seq_len(num_doors), c(car_door, chosen_door))
      
      if (length(free_doors) <= 1) {
        open_doors <- free_doors
      } else {
        open_doors <- sample(free_doors, (num_doors - 2))
      }
      
      switch_door <- setdiff(seq_len(num_doors), c(chosen_door, open_doors))
      
      win_stay <- chosen_door == car_door
      win_switch <- switch_door == car_door
      
      list(
        car_door = car_door,
        chosen_door = chosen_door,
        open_doors = open_doors,
        switch_door = switch_door,
        win_stay = win_stay,
        win_switch = win_switch
      )
    }
    
    observeEvent(input$run_single_simulation, {
      num_doors <- input$num_doors
      chosen_door <- as.numeric(input$initial_choice)
      result <- run_simulation(num_doors, chosen_door)
      
      output$result <- renderPrint({
        cat("The car was behind door:", result$car_door, "\n")
        cat("You chose door", result$chosen_door, "\n")
        cat("Monty opened doors:", paste(result$open_doors, collapse = ", "), "\n\n")
        cat("Switch to door:", result$switch_door, "\n")
        cat("Win by staying with your choice:", result$win_stay, "\n")
        cat("Win by switching your choice:", result$win_switch, "\n")
      })
    })
    
    observeEvent(input$run_multiple_simulations, {
      num_doors <- input$num_doors
      chosen_door <- as.numeric(input$initial_choice)
      results <- replicate(100, run_simulation(num_doors, chosen_door), simplify = FALSE)
      
      win_stay_count <- sum(sapply(results, function(result) result$win_stay))
      win_switch_count <- sum(sapply(results, function(result) result$win_switch))
      
      output$result <- renderPrint({
        cat("Results of 100 simulations:\n")
        cat("Wins by staying with initial choice:", win_stay_count, "\n")
        cat("Wins by switching choice:", win_switch_count, "\n")
      })
    })
    
    observe({
      min_num_doors <- input$min_num_doors
      num_car_doors <- input$num_car_doors
      
      updateSliderInput(
        session,
        "num_car_doors",
        max = min_num_doors - 1,
        value = num_car_doors
      )
      
      switch_win_chance <- function(doors, cars) {
        (doors - cars) / doors
      }
      max_num_doors <- 200
      
      req(min_num_doors)
      x_axis <- min_num_doors:max_num_doors
      
      output$win_plot <- renderPlot({
        plot(x_axis,
             switch_win_chance(x_axis, num_car_doors),
             ylim = c(0, 1),
             xlab = "Total number of doors",
             ylab = "Win chance by switching",
             col = "orange",
             pch = 20
        )
      })
    })
  })
}
