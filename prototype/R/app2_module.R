library(styler)
app2UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Monty Hall Problem"),
    p("The Monty Hall problem is a well known probability puzzle based on a game
    show scenario.A contestant is presented with three doors, behind one of
    which is a car (the prize) and behind the other two, goats(blank). After the
    contestant picks a door, the host(Monty Hall), who knows what's behind all
    the doors, opens one of the remaining doors to reveal a goat. The contestant
    is then given the option to stick with their original choice or switch to
    the other unopened door.Surprisingly, switching doors increases the
    contestant's chances of winning the car from 1/3 to 2/3."),
    fluidRow(
      box(
        width = 12, title = "App 2 Content", status = "primary",
        solidHeader = TRUE,
        sliderInput(ns("num_doors"),
          "Number of doors:",
          min = 3,
          max = 20,
          value = 3,
          ticks = TRUE
        ),
        actionButton(ns("run_simulation"), "Run Simulation"),
        uiOutput(ns("door_selection")),
        verbatimTextOutput(ns("result")),
      ),
      box(
        width = 12, title = "App 2 Content", status = "primary",
        solidHeader = TRUE,
        # second diagramm
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

    # first "diagramm"
    observeEvent(input$run_simulation, {
      # initial input
      num_doors <- input$num_doors
      chosen_door <- as.numeric(input$initial_choice)

      # assigning car door
      car_door <- sample(seq_len(num_doors), 1)

      # computing which doors to open
      free_doors <- setdiff(seq_len(num_doors), c(car_door, chosen_door))

      if (length(free_doors) <= 1) {
        open_doors <- free_doors
      } else {
        open_doors <- sample(free_doors, (num_doors - 2))
      }

      # computing which door to switch to
      switch_door <- setdiff(seq_len(num_doors), c(chosen_door, open_doors))

      # computing strategy result
      win_stay <- chosen_door == car_door
      win_switch <- switch_door == car_door

      # printing results
      output$result <- renderPrint({
        cat("The car was behind door:", car_door, "\n")
        cat("You chose door", chosen_door, "\n")
        cat("Monty opened doors:", paste(open_doors, collapse = ", "), "\n\n")
        cat("Switch to door:", switch_door, "\n")
        cat("Win by staying with your choice:", win_stay, "\n")
        cat("Win by switching your choice:", win_switch, "\n")
      })
    })

    # second diagramm
    observe({
      # initial input
      min_num_doors <- input$min_num_doors
      num_car_doors <- input$num_car_doors

      # updating the slider
      updateSliderInput(
        session,
        "num_car_doors",
        max = min_num_doors - 1,
        value = num_car_doors
      )


      # calculate winning chance on switch
      switch_win_chance <- function(doors, cars) {
        (doors - cars) / doors
      }
      # upper bound x axis
      max_num_doors <- 200

      req(min_num_doors)
      # creating x axis
      x_axis <- min_num_doors:max_num_doors

      # plotting win chances
      output$win_plot <- renderPlot({
        plot(x_axis,
          switch_win_chance(x_axis, num_car_doors),
          ylim = c(0, 1),
          xlab = "total number of doors",
          ylab = "win chance by switching",
          col = "orange",
          pch = 20
        )
      })
    })
  })
}
