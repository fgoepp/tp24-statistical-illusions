library(shiny)
library(bslib)
library(plotly)
library(shinyBS)

app3UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("the Two Children Problem"),
    fluidRow(
      box(
        width = 12, title = "The visualization", status = "primary",
        solidHeader = TRUE,
        layout_sidebar(
          sidebar = sidebar(
            tags$p("The two children problem, also known as the boy or girl paradox, is a well-known issue in probability theory. Its initial formulation dates back to October 1959, when it was published by Martin Gardner in the 'Mathematical Games' column in Scientific American, an American popular science magazine."),
            tags$p("Martin Gardner was an American writer known for his work in popular mathematics and science, as well as his interests in magic and scientific skepticism. The problem consists of two seemingly simple questions that have sparked significant controversy and debate among scientists. Since its introduction, many have shared their interpretations and even created variations of this problem."),
            tags$p("The fact that different solutions for this problem persist today underscores its intriguing nature and lasting impact. It has significantly contributed to our understanding of mathematics, statistics, and, to some extent, our own cognitive processes.")
          ),
          sidebar_width = 3,
          fluidRow(
            column(
              12,
              p("here's how the problem goes:"),
              p("Imagine youre speaking with someone. They tell you that they that they have" 
                ,tags$span("two", style = "color: blue; font-weight: bold;"), "children. Then they ask you:")
            ),
            column(12, selectInput(
              inputId = ns("dropdown_menu"),
              label = NULL,
              choices = list("QUESTION1: At least one of them is a boy. What is the probability that both children are boys?", 
                             "QUESTION2: The older child is a boy. What is the probability that both children are boys?", 
                             "Problem 3"),
              selected = "At least one of them is a boy. What is the probability that both children are boys?"
            )),
            column(12, "Now these two questions seem easy enough! but ofcourse the devil is in the details. try and solve the questions yourself", style = "margin-bottom: 20px;"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'QUESTION1: At least one of them is a boy. What is the probability that both children are boys?'", ns("dropdown_menu")),
              fluidRow(
                column(
                  3,
                  img(src = "BB.png", width = "100%"),
                  tags$p("")
                ),
                column(
                  3,
                  img(src = "GG-crossed.png", width = "75%"),
                  tags$p("is crossed out because there should be atleast one boy!")
                ),
                column(
                  3,
                  img(src = "Bg.png", width = "100%"),
                  tags$p("")
                ),
                column(
                  3,
                  img(src = "Gb.png", width = "65%"),
                  tags$p("")
                )
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'QUESTION2: The older child is a boy. What is the probability that both children are boys?'", ns("dropdown_menu")),
              fluidRow(
                column(
                  3,
                  img(src = "BB.png", width = "100%"),
                  tags$p("")
                ),
                column(
                  3,
                  img(src = "GG-crossed.png", width = "75%"),
                  tags$p("is crossed out because the oldest child should be a boy!")
                ),
                column(
                  3,
                  img(src = "Bg.png", width = "100%"),
                  tags$p("")
                ),
                column(
                  3,
                  img(src = "Gb-crossed.png", width = "65%"),
                  tags$p("is crossed out because the oldest child should be a boy!")
                )
              )
            ),
            fluidRow(
              column(
                12,
                actionButton(inputId = ns("show_answer"), label = "Show Answer"),
                textInput(inputId = ns("guess_probability"), label = "Guess the Probability", placeholder = "Enter a number"),
                actionButton(inputId = ns("submit_guess"), label = "Submit")
              )
            ),
            fluidRow(
              column(
                12,
                p(" Hopefully you have already clicked on the 'show_answer' answer button and seen the conflicting views
                  regarding the two questions, and this is exactly the first lesson we can learn from the two children problem.
                  Namely the fact that when formulating a problem, we have to make sure that we are as excat and clear as possible 
                  and that the smallest room for interpetaion can lead to the problem having sometimes vastly different answers. This highlights the 
                  importance of",tags$span("clarity and Accuracy ",style = "color: purple; font-weight: bold;"),"in probabilty and math", style = "margin-top: 20px; margin-left: 20px;")
              )
            )
          )
        )
      )
    ),
    fluidRow(
      box(
        width = 12, title = "The graphs and plots", status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(
            12,
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
        if (option == "QUESTION1: At least one of them is a boy. What is the probability that both children are boys?") {
          showModal(modalDialog(
            title = "Answer",
            p("1/3 => but why?? now in the visualization you can see the valid combinations of two children. we have in total three 
            valid combinations, of which we want only one combination -> giving us a probability of 1/3. "),
            p("but wait a lot of people have argued that the probability is actually 1/2. if we assume that the phrase 'atleast
              one of them is a boy' hints that during our conversation, we have actually seen or atleast know which of the two children is a boy,
              then the change of the other child being a boy is coin toss or 1/2 ! in some variations this first question is phrased as '(atleast) one of them is a boy. What is the probability that both children are boys?'
              in which case some could even say that in a day to day conversation, if we are told that Mr. Jones has two kids and one of them is a boy, well then the other one is a girl! obviously if the other one was a boy 
              as well then they would have simply said that 'they have two boys' from the get go. So therefor the probability that Mr.Jones has two boys is zero!!")
          ))
        } else if (option == "QUESTION2: The older child is a boy. What is the probability that both children are boys?") {
          showModal(modalDialog(
            title = "Answer",
            p("1/2 => but why?? now in the visualization you can see the valid combinations of two children. we have in total two 
            valid combinations, of which we want only one combination -> giving us a probability of 1/2"),
            p("but wait in this case some people could say that in a day to day conversation, if we are told that Mr. Jones has two kids and one of them is a boy, well then the other one is a girl! obviously if the other one was a boy 
              as well then they would have simply said that 'they have two boys' from the get go. So therefor the probability that Mr.Jones has two boys is zero!!")
          ))
        }
        # Add more conditions in the future
      }
    })
    
    observeEvent(input$submit_guess, {
      # Check if the guessed probability matches the answer
      option <- input$dropdown_menu
      answer_one <- list("1/3", "1 / 3","1/ 3", "1 /3", "one third", "0.33","0.3","0.333")
      answer_two <- list("1/2", "1 / 2","1/ 2", "1 /2", "half", "0.5")
      guess <- input$guess_probability
      if (!is.null(guess) && guess != "" && !is.na(guess)) {
        if (((guess %in% answer_one) && option == "QUESTION1: At least one of them is a boy. What is the probability that both children are boys?") ||
            ((guess %in% answer_two) && option == "QUESTION2: The older child is a boy. What is the probability that both children are boys?")) {
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
      } else{
        showModal(modalDialog(
          title = "Not valid",
          "please enter a valid answer!"
        ))
      }
    })
    
    plot_data <- reactive({
      n <- input$n_children
      data.frame(
        Case = c(
          paste(n, "children"),
          paste(n, "children, at least one is a boy"),
          paste(n, "children, the oldest is a boy")
        ),
        #Probability = c(1 / (2^n), 1 / (2^n - 1), 1 / 2^(n-1) )
        Probability = c(round(1 / (2^n), digits = 6), 
                        round(1 / (2^n - 1), digits = 6), 
                        round(1 / 2^(n-1), digits = 6) )
      )
    })
    
    output$bar_plot <- renderPlotly({
      data <- plot_data()
      plot_ly(data,
              x = ~Case, y = ~Probability, type = "bar", text = ~ paste("Probability:", Probability),
              hoverinfo = "text"
      ) %>%
        layout(
          title = "Bar Plot of Probabilities",
          xaxis = list(title = "Obscurity of information"),
          yaxis = list(title = "Probability")
        ) %>%
        config(displayModeBar = FALSE)
    })
    
    output$line_plot <- renderPlotly({
      data <- plot_data()
      plot_ly(data,
              x = ~Case, y = ~Probability, type = "scatter", mode = "lines+markers", text = ~ paste("Probability:", Probability),
              hoverinfo = "text"
      ) %>%
        layout(
          title = "Line Plot of Probabilities",
          xaxis = list(title = "Obscurity of information"),
          yaxis = list(title = "Probability")
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}