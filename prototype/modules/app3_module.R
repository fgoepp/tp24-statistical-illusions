library(shiny)
library(bslib)
library(plotly)
library(shinyBS)


question1 <- "QUESTION1: At least one of the two children is a boy.
What is the probability that both children are boys?"
question2 <- "QUESTION2: The older child is a boy. What is the probability that
both children are boys?"
question3 <- "QUESTION3: At least one of the two children is a boy and born on
a tuesday. What is the probability that both children are boys?"
app3UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    h1("The Two Children Problem"),
    p(
      "The Two Children Problem is a classic puzzle in probability theory that
        demonstrates how tricky statistical thinking can be. It usually comes as a set
        of two simple questions that, despite their simplicity, challenge our everyday
        intuition and reveal how complex probability can be. By examining the relationship
        between what we know and the possible outcomes, the Two Children Problem becomes
        a fascinating exercise in logical thinking and understanding statistics."
    ),
    "The problem is as follows:",
    p(
      "Imagine you're having a nice conversation with your neighbor Mr. Jones. he
      tells you that he has two children.The two questions then are the following 
      (assume that the probability of being born a boy or girl is equal):"
    ),
    p(
      HTML("<i>if (at least) one of the two children is a boy. What is the probability
      that both children are boys?</i> <br>
           <i>if the older child is a boy. What is the probability that both
           children are boys?</i>")
    ),
    # =========== BOX NR. 1 ===========
    fluidRow(
      box(
        width = 12, title = "Main Setup: The Classic Variation", status = "primary",
        solidHeader = TRUE,
        fluidRow(
          column(12,
            style = "margin-left: 3px;",
            selectInput(
              inputId = ns("dropdown_menu"),
              label = NULL,
              choices = list(
                question1,
                question2
              ),
              selected = question1
            )
          ),
          column(7,
            style = "margin-left: 5px;",
            "Now, these two questions seem easy enough, but of course, the devil
            is in the details. Try and solve the questions yourself:",
            style = "margin-bottom: 20px;"
          ),
          column(2,
            checkboxInput(ns("show_hint"), "show visual hint", FALSE),
            style = "margin-top: -9px; margin-right: -20px;"
          ),
          column(12, uiOutput(ns("classic_images"))),
          fluidRow(
            fluidRow(
              column(
                5,
                style = "margin-left: 34px;",
                textInput(
                  inputId = ns("guess_probability"), label = "Guess the Probability",
                  placeholder = "Enter a fraction (e.g., 5/8)"
                )
              ),
              column(
                6,
                style = "margin-top: 25.5px;",
                actionButton(inputId = ns("submit_guess"), label = "Submit")
              )
            ),
            column(
              12,
              style = "margin-left: 20px;",
              actionButton(inputId = ns("show_answer"), label = "Show Answer")
            )
          ),
          fluidRow(
            column(
              12,
              p("
                Hopefully, you've already clicked the 'Show Answer' button and 
                seen the conflicting views regarding the two questions. This is 
                exactly the first lesson we can learn from the Two Children Problem: 
                the fact that when formulating a problem, we have to make sure that
                we are as exact and clear as possible. Even the smallest room for 
                interpretation can lead to the problem having vastly different answers.
                This highlights the importance of ",
                tags$span("clarity, accuracy, and interpretation ",
                  style = "color: purple; font-weight: bold;"
                ),
                "in probability and math.",
                style = "margin-top: 20px; margin-left: 20px;"
              ),
              p(
                "Now, just to be clear, we assume the pure mathematical meaning
                of the phrase 'one of them,' meaning that the expression equals 
                'at least one of them.' and consider the probability of a child 
                being born a male or a female to be equally likely. Additionally
                , using our mathematical approach, because it isn't explicitly
                mentioned in Question 1 that we have seen the child who is a boy,
                we assume that it truly is by chance that at least one of them
                is a boy.",
                style = "margin-left: 20px;"
              )
            )
          )
        )
      )
    ),
    # =========== BOX NR. 2 ===========
    fluidRow(
      box(
        width = 12, title = "Secondary Setup: The Tuesdayboy Variation", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        p(HTML("The Tuesday Boy Problem is an intriguing variation and extension 
        of the classic Two Children Problem. It introduces a third question to 
        our setup, which is: <br>
        Just like before, you are speaking with Mr. Jones, who tells you he has 
        two children. The new question is: <br> <br>
        <i>if least one of the two children is a boy and born on a tuesday,
        What is the probability that both children are boys?</i> <br>")),
        checkboxInput(ns("show_gif"), "show GIF (uncheck to see still image)", TRUE),
        uiOutput(ns("gif_ui")),
        column(
          12,
          p("The illustration helps showcase the possible valid combinations 
            (where at least one boy is born on a Tuesday) using blue squares. 
            Among these, the combinations we are interested in (where both children 
            are boys) are highlighted with green squares.", style = "margin-top: 10px;"),
          p(
            "This example once again underscores the importance of being",
            tags$span("accurate and precise", style = "color: purple; font-weight: bold;"),
            "accurate and precise. Adding seemingly small details, such as the 
            day of birth, can significantly alter the probability and make this
            question different from the very similar Question 1."
          )
        )
      )
    ),
    # =========== BOX NR. 3 ===========
    fluidRow(
      box(
        width = 12, title = "The graphs and plots", status = "primary", collapsible = TRUE, collapsed = TRUE,
        solidHeader = TRUE,
        fluidRow(
          column(
            12,
            p(
              "In this section we will look at the previous three question plus 
              an additional new question but this time not just for 2 children but
              rather ",
              tags$span("n children ", style = "color: purple; font-weight: bold;"),
              "and plot each probability."
            ),
            p(
              HTML
              ("The questions are again listed here (the first one is the newly
              added variation) : <br> <i>
              _ Mr. Jones has n children. What's the probability that all n children 
              are boys? <br>
              _ Mr. Jones has n children and at least one of them is a boy. What's
              the probability that all n children are boys? (Question 1)<br>
              _ Mr. Jones has n children and at least one of them is a boy born 
              on a tuesday. What's the probability that all n children are boys? 
              (Tuesdayboy)<br>
              _ Mr. Jones has n children and the oldest child is a boy. What's
              the probability that all n children are boys? (Question 2) </i>")
            ),
            p("now you can use the slider to change the number of children we
              have (and still calculate the probabilty of ALL of children being boys!)
              and see that the pattern doesn't change!")
          ),
          column(12,
            sliderInput(ns("n_children"), "Number of children:", min = 2, max = 10, value = 2, width = "70%"),
            style = "margin-left: 180px; margin-top:30px;"
          ),
          fluidRow(
            column(12, plotlyOutput(ns("bar_plot"), width = "80%"), style = "margin-left: 100px;")
          )
        )
      )
    ),
    # =========== BOX NR. 4 ===========
    fluidRow(
      box(
        width = 12, title = "Mathematical Background ", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        p("In this section we will take a look at the math behind the two chidlren
          problem and its variation and how the probabilities were calculated in
          the 'The graphs and plots' section."),
        p(HTML("In the upper section we took a look at four different question: <br>
               if a family has n children: <br>

               1- and at least one of them is a boy. What is the probability
               that all children are boys? <br>
               2- and the older child is a boy. What is the probability that
               all children are boys? <br>
               3- and at least one of them is a boy born on tuesday. What is the
               probability that all children are boys? <br>
               4- What is the probability that all children are boys? <br> <br> <br>

               1- now normally for n children we have \\(\\ 2^n\\) combinations
               (2 becausewe are concidering a child being either a boy or a girl) but in
               addition we know that atleast one child is a boy, which eliminates
               the combination where all children are girls which gives us
               \\(\\ 2^n - 1\\) combinations. seeing as we're looking for the
               case that all children are boys, and that leaves us with only one
               case then the probability is:
               $$\\frac{1}{2^n - 1} $$ <br>

               2- now because the OLDEST child is a boy, it is determined that the
               oldest child has 1 possibility (being a boy) which leaves the other
               n-1 children to be either girls or boys giving us \\(\\ 2^{n-1}\\)
               combinations and with again one desired combination (where all children
               are boys) we get the probability of: $$\\frac{1}{2^{n - 1}} $$<br>

               3- in this case we treat (gender + 7 days of the week) as a one entity
               in order to easier solve this problem. first let's take a look at our
               sought out combinations: <br>
               'all children being boys and atleast one born on tuesday' which
               we can calculate via its compliment:
               all boys - all boys and no boy born on a tuesday = \\(\\ 7^n - 6^n\\).
               in the first term we have 7 because we're looing for 1 gender (boy)
               and 7 days:\\(\\ 1\\cdot7\\) and in the second one we have 1
               gender and 6 days (no tuesday):\\(\\ 1\\cdot6\\).<br>

               now lets take a look at all combinations which is n children with
               atleast one boy on a tuesday. again let's use the compliment:
               everyone - everyone except boys on tuesday = \\(\\ 14^n - 13^n\\).
               in the first term we have 14 because we're concidering two genders
               and 7 days: \\(\\ 2\\cdot7\\). in the second term we have 13 because
               we're concidering no boys on tuesdays meaning that girls and 7 days
               (7 possibilities) and boys and 6 days (6 possibilities) and together:
               \\(\\ 1\\cdot7 + 1\\cdot6 = 13\\). in total the desired combination over
               the entire combinations gives us the probability of:
               $$\\frac{7^n - 6^n}{14^n - 13^n} $$<br>

               4- and here we are atl last where we have simply \\(\\ 2^n\\) combinations
               without any restraints and as always one desired outcome. the probability is:
               $$\\frac{1}{2^n} $$<br>
               "))
      )
    ),
    # =========== BOX NR. 5 ===========
    fluidRow(
      box(
        width = 6, title = "Historical background", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        p(
          tags$p("The two children problem, also known as the boy or girl paradox,
                 is a well-known problem in probability theory. Its initial formulation
                 dates back to October 1959, when it was published by Martin Gardner
                 in the 'Mathematical Games' column in Scientific American,
                 an American popular science magazine."),
          tags$p("Martin Gardner was an American writer known for his work in
                 popular mathematics and science, as well as his interests in
                 magic and scientific skepticism. The problem consists of two
                 seemingly simple questions that have sparked significant controversy
                 and debate among scientists. Since its introduction, many have
                 shared their interpretations and even created variations of this problem."),
          tags$p("The fact that different solutions for this problem persist
                 today underscores its intriguing nature and lasting impact.
                 It has significantly contributed to our understanding of
                 mathematics, statistics, and, to some extent, our own
                 cognitive processes.")
        )
      ),
      box(
        width = 6, title = "References", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        fluidRow(column(
          10,
          p("Boy or girl paradox - Wikipedia"),
          p("Youtube channels:"),
          p("Kevin Olding - Mathsaurus | The Tuesday Boy Problem"),
          p("ThePuzzlr | The Boy Or Girl Paradox")
        ))
      )
    )
  )
}

app3Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # toggle and show pictures in the BOX NR. 1
    output$classic_images <- renderUI({
      hint <- input$show_hint
      option <- input$dropdown_menu
      if (option == question1 && hint) {
        fluidRow(
          style = "margin-left: 20px;",
          column(3, img(src = "/two_children/BB.png", width = "100%")),
          column(
            3, img(src = "/two_children/GG-crossed.png", width = "75%"),
            tags$p("is crossed out because there should be atleast one boy!")
          ),
          column(3, img(src = "/two_children/Bg.png", width = "100%")),
          column(3, img(src = "/two_children/Gb.png", width = "65%"))
        )
      } else if (option == question2 && hint) {
        fluidRow(
          style = "margin-left: 20px;",
          column(3, img(src = "/two_children/BB.png", width = "100%")),
          column(
            3, img(src = "/two_children/GG-crossed.png", width = "75%"),
            tags$p("is crossed out because the oldest child should be a boy!")
          ),
          column(3, img(src = "/two_children/Bg.png", width = "100%")),
          column(
            3, img(src = "/two_children/Gb-crossed.png", width = "65%"),
            tags$p("is crossed out because the oldest child should be a boy!")
          )
        )
      }
    })
    
    # toggle GIF
    output$gif_ui <- renderUI({
      if (input$show_gif) {
        img(src = "/two_children/tuesdayboy.gif", width = "90%")
      } else {
        img(src = "/two_children/tuesdayboy.JPG", width = "90%")
      }
    })

    # =========== DISPLAY ANSWERS ===========
    observeEvent(input$show_answer, {
      option <- input$dropdown_menu
      if (!is.null(option) && !is.na(option)) {
        if (option == question1) {
          showModal(modalDialog(
            title = "Answer",
            p(HTML("<h2> 1/3 </h2> <br>
            But why? Now, if you used the visual hint, you saw the valid combinations
                   for two children. We have, in total, three valid combinations (BB, BG, GB), 
                   of which we want only one combination (BB), giving us a probability
                   of 1/3.")),
            p("but wait a lot of people have argued that the probability
            is actually 1/2. if we assume that the phrase 'atleast
              one of them is a boy' hints that during our conversation, we have
              actually seen or atleast know which of the two children is a boy,
              then the change of the other child being a boy is coin toss or 1/2!
              in some variations this first question is phrased as '(atleast) one
              of them is a boy. What is the probability that both children are boys?'
              in which case some could even say that in a day to day conversation,
              if we are told that Mr. Jones has two kids and one of them is a boy,
              well then the other one is a girl! obviously if the other one was a boy
              as well then they would have simply said that 'they have two boys'
              from the get go. So therefor the probability that Mr.Jones has
              two boys is zero!!")
          ))
        } else if (option == question2) {
          showModal(modalDialog(
            title = "Answer",
            p(
              HTML("<h2> 1/2 </h2> <br>
              But why? if you used the visual hint, you saw the valid combinations
                   for two children. We have, in total, two valid combinations (BB, BG), 
                   of which we want only one combination (BB), giving us a probability
                   of 1/2."),
              p("but wait in this case some people could say that in a day to day
            conversation, if we are told that Mr. Jones has two kids and one of
            them is a boy, well then the other one is a girl! obviously if the
            other one was a boy as well then they would have simply said that
              'they have two boys' from the get go. So therefor the probability
              that Mr. Jones has two boys is zero!!")
            )
          ))
        }
      }
    })

    # =========== CHECK ANSWERS ===========
    observeEvent(input$submit_guess, {
      option <- input$dropdown_menu
      answer_one <- list("1/3", "0.33", "0.3", "0.333")
      answer_two <- list("1/2", "0.5")
      guess <- input$guess_probability
      guess <- gsub("\\s+", "", guess)
      if (!is.null(guess) && guess != "" && !is.na(guess)) {
        if (((guess %in% answer_one) && option == question1) ||
          ((guess %in% answer_two) && option == question2)) {
          showModal(modalDialog(
            title = "Correct",
            "Yup, you got it!"
          ))
        } else {
          showModal(modalDialog(
            title = "Incorrect",
            "Not quite right, try again!"
          ))
        }
      } else {
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
          paste(n, "children, at least one is a boy on a Tuesday"),
          paste(n, "children, the oldest is a boy")
        ),
        Probability = c(
          round(1 / (2^n), digits = 6),
          round(1 / (2^n - 1), digits = 6),
          round((7^n - 6^n) / (14^n - 13^n), digits = 6),
          round(1 / 2^(n - 1), digits = 6)
        )
      )
    })

    output$bar_plot <- renderPlotly({
      colors <- c("green", "blue", "orange", "purple")
      data <- plot_data()
      plot_ly(data,
        x = ~Case, y = ~Probability, type = "bar", text = ~ paste("Probability:", Probability),
        hoverinfo = "text",
        marker = list(color = colors)
      ) %>%
        layout(
          title = "Bar Plot of Probabilities",
          xaxis = list(title = "Obscurity of information"),
          yaxis = list(title = "Probability")
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
