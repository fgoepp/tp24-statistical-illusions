library(shiny)
library(styler)

app2UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    h1("Monty Hall Problem"),
    # introduction
    fluidRow(
      column(
        12,
        p("The Monty Hall problem is a classic probability puzzle that
          challenges our intuition. Traditionally, it involves a game show
          where a contestant must choose one of three doors, behind one of which
          is a car (the prize) and behind the others are goats (non-prizes).
          After the contestant picks a door, the host, Monty Hall, who knows
          what's behind each door, opens one of the remaining doors to reveal
          a goat. The contestant is then given the option to switch to the other
          unopened door or stay with their initial choice."),
        p("Which option do you think will perform the best: staying or switching
          ?"),
        p("In this variation of the Monty Hall problem, we extend the scenario
          to include a variable number of total doors while still maintaining
          only one car door. This generalization allows us to gain a deeper
          understanding of the underlying probability. By adjusting the number
          of doors, we can explore how the probabilities change and observe any
          emerging patterns."),
        p("Can you discover a pattern for \\(N\\) doors in general?")
      ),
      # Main Setup
      box(
        title = "Main Setup: The Basic Variation",
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
        radioButtons(ns("switch_or_stay"), "Switch or Stay:",
          choices = c("Switch", "Stay"),
          selected = "Switch"
        ),
        actionButton(ns("run_single_simulation"), "Run single Simulation"),
        actionButton(ns("run_multiple_simulations"), "Run 100 Simulations"),
        verbatimTextOutput(ns("result"))
      ),
      # Mathematical Background
      fluidRow(
        column(
          6,
          box(
            title = "Mathematical Background",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            p("In the classic Monty Hall problem, there are 3 doors.
              Behind one of these doors is a car (the prize), and behind the
              other two doors are goats (non-prizes). Here's how the problem
              plays out with \\(N = 3\\):"),
            p(tags$b("Initial Choice:"), "The contestant picks one of the 3
              doors. Let's say they pick door 1."),
            p("- The probability that door 1 has the car is
              \\( \\frac{1}{3} \\)."),
            p("- The probability that door 1 has a goat is
              \\( \\frac{2}{3} \\)."),
            p(tags$b("Monty Opens Doors:")),
            p("Monty Hall, who knows what's behind each door, opens one of the
              other two doors, revealing a goat. Suppose he opens door 3."),
            p("This leaves doors 1 and 2 unopened."),
            p(tags$b("Switching or Staying:")),
            p("- Staying: If the contestant stays with door 1, the probability
              of winning the car is still \\( \\frac{1}{3} \\)."),
            p("- Switching: If the contestant switches to door 2, the
              probability of winning the car is \\( \\frac{2}{3} \\)."),
            p("This switching probability is higher because:"),
            p("- If the initial choice (door 1) was incorrect (which happens
              with probability \\( \\frac{2}{3} \\)), switching will lead to the
              car."),
            p("- If the initial choice was correct (which happens with
              probability \\( \\frac{1}{3} \\)), switching will lead to a
              goat."),
            p("The decision tree diagram illustrates these probabilities
              clearly:"),
            img(src = "monty_hall/desiciontree.png", height = "200px"),
            p("In the diagram, the upper branch shows the probability of
              choosing a car door (\\( \\frac{1}{3} \\)), which results in
              losing if you switch. The lower branch shows the probability
              of choosing a goat door (\\( \\frac{2}{3} \\)), which results
              in winning if you switch."),
            p("The overall probability of winning by switching is calculated
              as follows:"),
            p("- The probability of initially choosing a goat door and then
              switching to the car: \\( \\frac{2}{3} \\times 1 =
              \\frac{2}{3} \\)."),
            p(h3("Generalized for any \\(N \\)")),
            p("Now that we understand the math behind the classic Monty Hall
            problem, its time to get back to our Main Setup, where there are
            \\( N \\) doors, with one door hiding a car
          and the remaining \\( N - 1 \\) doors hiding goats."),
            p(tags$b("Initial Choice:"), "The contestant initially picks one
              of the \\( N \\) doors."),
            p("- The probability that the chosen door has the car behind it is
              \\( \\frac{1}{N} \\)."),
            p("- The probability that the chosen door has a goat behind it is
              \\( \\frac{N - 1}{N} \\)."),
            p(tags$b("Monty Opens Doors:")),
            p("Monty Hall, knowing what is behind each door, opens \\( N - 2 \\)
            doors, all revealing goats,
          which leaves two doors unopened: the contestant’s initially chosen
              door and one other door."),
            p(tags$b("Switching or Staying:")),
            p("- Staying: If the contestant stays with their initial choice, the
              probability of winning the car remains \\( \\frac{1}{N} \\)."),
            p("- Switching: If the contestant switches to the other unopened
              door, the probability of winning the car is
              \\( \\frac{N - 1}{N} \\)."),
            p("This switching probability is higher because if the initial
            choice was incorrect (which happens with probability
            \\( \\frac{N - 1}{N} \\)),
          switching will always lead to the car. On the other hand, if the
          initial choice was correct (which happens with probability
          \\( \\frac{1}{N} \\)),
          switching will lead to a goat.")
          ),
        ),
        column(
          6,
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
          probability and conditional probability."),
            p("The problem is named after Monty Hall, the original host of the
            American television game show \"Let\'s Make a Deal.\"
          On the show, contestants were often presented with a choice between
          three doors, behind one of which was a valuable prize,
          such as a car, while behind the other two were goats.
          The game\'s twist, where Monty would reveal a goat behind one of the
          unchosen doors and then offer the contestant a chance to switch their
              choice, fascinated viewers and sparked discussions about the best
              strategy."),
            p("Vos Savant's column on the Monty Hall problem drew thousands
            of letters, many of which criticized her solution and insisted that
            switching doors would not improve the odds of winning the car.
            This widespread backlash highlighted how counterintuitive the
            correct answer seemed, even to those well-versed in mathematics and
              probability theory."),
            p("As more mathematicians and statisticians analyzed the problem,
          they confirmed that vos Savant's recommendation to always switch doors
          was indeed the optimal strategy.This revelation turned the Monty Hall
          problem into a classic example in probability theory, demonstrating
              how human intuition can often lead us astray in understanding
              probabilities and decision-making under uncertainty."),
            p("Over time, the Monty Hall problem has been used in various
            educational settings to teach concepts
          such as conditional probability, Bayesian reasoning, and the
          importance of revising our beliefs in light of new information.
          It remains a popular and enduring puzzle that continues to intrigue
              and educate people about the complexities of probability.")
          ),
        ),
      ),
      # Secondary Setup
      box(
        title = "Secondary Setup: Advanced Variation Exploration",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        p("Now that we understand the math behind the Monty Hall problem, let's
        look at a new twist.
          In this new Secondary Setup, we will also be able to adjust the number
          of car doors.
          Dive in to see how this change impacts which strategy to choose. Can
          you figure out, which value determines the better strategy now?"),
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

      # References
      box(
        title = "References: Sources and further material",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
        p(HTML("<a href = https://youtu.be/4Lb-6rxZxx0> Numberphile </a>")),
        p(HTML("<a href = https://www.untrammeledmind.com/2018/11/monty-hall-problem-and-variations-intuitive-solutions/> Untrammeled Mind </a>")),
        p(HTML("<a href = https://www.businessinsider.com/the-monty-hall-problem-2013-3#-20> Business Insider </a>")),
        p(HTML("<a href = https://en.wikipedia.org/wiki/Monty_Hall_problem> Wikipedia </a>")),
        p("Tools used: ChatGPT, DeepL")
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

    run_simulation <- function(num_doors, chosen_door, switch) {
      car_door <- sample(seq_len(num_doors), 1)

      # Monty opens all doors with goats except for
      # the chosen one and one other door
      goat_doors <- setdiff(seq_len(num_doors), car_door)
      goat_doors <- setdiff(goat_doors, chosen_door)

      if (length(goat_doors) > 1) {
        open_doors <- sample(goat_doors, (num_doors - 2))
      } else {
        open_doors <- goat_doors
      }
      open_doors <- sort(open_doors)

      remaining_doors <- setdiff(seq_len(num_doors), c(open_doors, chosen_door))
      switch_door <- remaining_doors[remaining_doors != chosen_door]

      final_choice <- if (switch) switch_door else chosen_door
      win <- final_choice == car_door

      list(
        car_door = car_door,
        initial_chosen_door = chosen_door,
        open_doors = open_doors,
        switch_door = switch_door,
        chosen_door = final_choice,
        win = win
      )
    }

    observeEvent(input$run_single_simulation, {
      num_doors <- input$num_doors
      chosen_door <- as.numeric(input$initial_choice)
      switch <- input$switch_or_stay == "Switch"
      result <- run_simulation(num_doors, chosen_door, switch)

      output$result <- renderPrint({
        cat("The car is behind door:", result$car_door, "\n")
        cat("You chose door:", result$initial_chosen_door, "\n")
        cat(
          "Monty then opened doors:",
          paste(result$open_doors, collapse = ", "), "\n"
        )
        if (switch) {
          cat("You switched to door:", result$switch_door, "\n", "\n")
        } else {
          cat("You stayed with your initial choice.", "\n", "\n")
        }
        if (result$win) {
          cat("You WON and recieved a car", "\n")
        } else {
          cat("You LOST and recieved a goat", "\n")
        }
      })
    })

    observeEvent(input$run_multiple_simulations, {
      num_doors <- input$num_doors
      chosen_door <- as.numeric(input$initial_choice)
      switch <- input$switch_or_stay == "Switch"
      results <- replicate(100,
        run_simulation(
          num_doors,
          chosen_door,
          switch
        ),
        simplify = FALSE
      )

      win_count <- sum(sapply(results, function(result) result$win))

      output$result <- renderPrint({
        cat("Results of 100 simulations:\n")
        cat("Wins:", win_count, "\n")
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

      car_to_goat_ratio <- function(doors, cars) {
        cars / (doors - cars)
      }

      ratio_breaks <- car_to_goat_ratio(x_axis, num_car_doors)
      corresponding_doors <- round(num_car_doors / ratio_breaks) + num_car_doors

      # Filter ratio breaks to every fourth value
      ratio_breaks_filtered <- ratio_breaks[seq(1,
        length(ratio_breaks),
        by = 4
      )]
      corresponding_doors_filtered <- corresponding_doors[seq(1,
        length(corresponding_doors),
        by = 4
      )]

      output$win_plot <- renderPlot({
        plot(x_axis,
          switch_win_chance(x_axis, num_car_doors),
          ylim = c(0, 1),
          xlab = "Total number of doors",
          ylab = "Win chance by switching",
          col = "orange",
          pch = 20
        )

        par(new = TRUE)
        axis(3,
          at = corresponding_doors_filtered,
          labels = round(
            ratio_breaks_filtered,
            2
          )
        )
        mtext("Car to Goat Ratio",
          side = 3,
          line = 3
        )

        abline(
          h = 0.5,
          col = "red",
          lty = 2
        )
        axis(2,
          at = 0.5,
          labels = "0.5",
          col.axis = "red",
          las = 1
        )
      })
    })
  })
}

shinyApp(ui = app2UI, server = app2Server)
