library(shiny)
library(bslib)
library(plotly)
library(shinyBS)


question1 <- "QUESTION1: At least one of them is a boy. What is the probability
that both children are boys?"
question2 <- "QUESTION2: The older child is a boy. What is the probability that
both children are boys?"
question3 <- "QUESTION3: At least one of them is a boy and born on a tuesday.
What is the probability that both children are boys?"
app3UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    h1("the Two Children Problem"),
    p(
      "The Two Children Problem is a classic puzzle in probability theory that
        shows how tricky statistical thinking can be. It usually comes as a set
        of two simple questions that, despite their simplicity, challenge our everyday
        intuition and reveal how complex probability can be. By looking at the relationship
        between what we know and the possible outcomes, the Two Children Problem becomes
        a fascinating exercise in logical thinking and understanding statistics."
    ),
    "The problem is as follows:",
    p(
      "Imagine youre speaking with someone.
        They tell you that they that they have
        two children. The two questions then are the following:"
    ),
    p(
      HTML("*if least one of the two children is a boy. What is the probability
      that both children are boys?* <br>
           *if the older child is a boy. What is the probability that both
           children are boys?*")
    ),
    # =========== BOX NR. 1 ===========
    fluidRow(
      box(
        width = 12, title = "The classic variation", status = "primary", collapsible = TRUE,
        solidHeader = TRUE, collapsed = TRUE,
        fluidRow(
          column(12, selectInput(
            inputId = ns("dropdown_menu"),
            label = NULL,
            choices = list(
              question1,
              question2
            ),
            selected = question1
          )),
          column(12, "Now these two questions seem easy enough! but ofcourse the
                 devil is in the details. try and solve the questions yourself",
            style = "margin-bottom: 20px;"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == '%s'", ns("dropdown_menu"), question1),
            fluidRow(
              style = "margin-left: 20px;",
              column(
                3,
                img(src = "/two_children/BB.png", width = "100%"),
                tags$p("")
              ),
              column(
                3,
                img(src = "/two_children/GG-crossed.png", width = "75%"),
                tags$p("is crossed out because there should be atleast one boy!")
              ),
              column(
                3,
                img(src = "/two_children/Bg.png", width = "100%"),
                tags$p("")
              ),
              column(
                3,
                img(src = "/two_children/Gb.png", width = "65%"),
                tags$p("")
              )
            )
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == '%s'", ns("dropdown_menu"), question2),
            fluidRow(
              style = "margin-left: 20px;",
              column(
                3,
                img(src = "/two_children/BB.png", width = "100%"),
                tags$p("")
              ),
              column(
                3,
                img(src = "/two_children/GG-crossed.png", width = "75%"),
                tags$p("is crossed out because the oldest child should be a boy!")
              ),
              column(
                3,
                img(src = "/two_children/Bg.png", width = "100%"),
                tags$p("")
              ),
              column(
                3,
                img(src = "/two_children/Gb-crossed.png", width = "65%"),
                tags$p("is crossed out because the oldest child should be a boy!")
              )
            )
          ),
          fluidRow(
            column(
              8,
              style = "margin-left: 10px;",
              actionButton(inputId = ns("show_answer"), label = "Show Answer"),
              textInput(
                inputId = ns("guess_probability"), label = "Guess the Probability",
                placeholder = "Enter a number"
              ),
              actionButton(inputId = ns("submit_guess"), label = "Submit")
            )
          ),
          fluidRow(
            column(
              12,
              p("
              Hopefully you have already clicked on the 'show_answer' answer
              button and seen the conflicting views
              regarding the two questions, and this is exactly the first lesson
              we can learn from the two children problem.
              Namely the fact that when formulating a problem, we have to
              make sure that we are as excat and clear as possible
              and that the smallest room for interpetaion can lead to the
              problem having sometimes vastly different answers. This highlights the
              importance of",
                tags$span("clarity,  Accuracy and interpretation ",
                  style = "color: purple; font-weight: bold;"
                ),
                "in probabilty and math.",
                style = "margin-top: 20px; margin-left: 20px;"
              ),
              p(
                "Now just to be clear ;) we assume the pure mathematical meaning
              of the phrase 'one of them'. meaning that the expression equals to
              'atleast one of them' and also again using our mathematical approach,
               because it isn't explicitly mentioned in QUESTION1 that we have
               seen the child that is a boy,
                we assume that truly is by chance atleast one of them a boy.",
                style = "margin-left: 20px;"
              )
            )
          )
        )
      )
    ),
    # =========== BOX NR. 2 ===========
    h2("The Tuesdayboy variation", style = "margin-top: -10px;"),
    p(HTML("The Tuesdayboy Problem is an interesting variation and extention of the
    classic problem. It introduces a third question to our setup namely:<br>
    Just like before a person has two children then <br>
    *if least one of the two children is a boy. What is the probability
           that both children are boys?*")),
    fluidRow(
      box(
        width = 12, title = "The Tuesdayboy variation", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        checkboxInput(ns("show_gif"), "show Gif (uncheck to see still image)", TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("show_gif")),
          img(src = "/two_children/tuesdayboy.gif", width = "90%")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("show_gif")),
          img(src = "/two_children/tuesdayboy.JPG", width = "90%")
        ),
        column(
          12,
          p("The illustration helps showcase the possible valid combinations
            (atleast one boy born on a tuesday) with blue squares and from those
            the combinations we're looking for (all n children being boys) with
            green squares."),
          p(
            "The importance of being",
            tags$span("accurate and exact", style = "color: purple; font-weight: bold;"),
            "is ever so present seeing as adding a bit more information such as being
            born on a tuesday can change the probability and make this question
            different from the very similar QUESTION1."
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
              "in this section we will see another side of the two children
              problem, namely a peculiar pattern which arises from the level of",
              tags$span("obscurity", style = "color: purple; font-weight: bold;"),
              "and",
              tags$span("certainty ", style = "color: purple; font-weight: bold;"),
              "of our expresion."
            ),
            p(
              "The more", tags$span("information",
                style = "color: purple; font-weight: bold;"
              ), "we give in our QUESTION,
              the higher the chance that our two children are boys. You can see in
              the graph below that probability of two childs being boys is highest
              when we have the most amount of information, namely in QUESTION2
              (we KNOW which child is the boy, namely the oldest. It
              is almost like we know one of the children completely). In the case
              of QUESTION1 we lost a bit of information by stating 'atleast one of'
              and that decreases our probability. In the case of the variant we see
              that Tuesday boy varient add a bit more information on top of QUESTION1 by
              stating that the boy is born on a tuesday. this is reflected in the graph
              and we see that we have a higher probability than Question1 but
              seeing as we are still not as certain as QUESTION2, our probabilty
              is inbetween the two. Finally the worst case scenario is when we have
              no information at all which is colored green where we simply
              calculate the probabilty of two children being boys without
              any additional information."
            ),
            p("now you can use the slider to change the number of children we
              have (and still calculate the probabilty of ALL of children being boys!)
              and see that the pattern doesn't change!")
          ),
          column(12,
            sliderInput(ns("n_children"), "Number of children:", min = 2, max = 10, value = 2, width = "70%"),
            style = "margin-left: 180px; margin-top:30px;"
          ),
          column(12, plotlyOutput(ns("bar_plot"), width = "80%"), style = "margin-left: 100px;")
        )
      )
    ),
    # =========== BOX NR. 4 ===========
    fluidRow(
      box(
        width = 12, title = "Mathematical Equation", status = "primary",
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
                 is a well-known issue in probability theory. Its initial formulation
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
        width = 6, title = "Refrences and material", status = "primary",
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
        if (option == question1) {
          showModal(modalDialog(
            title = "Answer",
            p("1/3 => but why?? now in the visualization you can see the
            valid combinations of two children. we have in total three
            valid combinations, of which we want only one combination ->
              giving us a probability of 1/3. "),
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
            p("1/2 => but why?? now in the visualization you can see the valid
            combinations of two children. we have in total two
            valid combinations, of which we want only one combination -> giving
              us a probability of 1/2"),
            p("but wait in this case some people could say that in a day to day
            conversation, if we are told that Mr. Jones has two kids and one of
            them is a boy, well then the other one is a girl! obviously if the
            other one was a boy as well then they would have simply said that
              'they have two boys' from the get go. So therefor the probability
              that Mr.Jones has two boys is zero!!")
          ))
        }
      }
    }) 

    observeEvent(input$submit_guess, {
      # Check if the guessed probability matches the answer
      option <- input$dropdown_menu
      answer_one <- list("1/3", "0.33", "0.3", "0.333")
      answer_two <- list("1/2", "0.5")
      guess <- input$guess_probability
      guess <- gsub("\\s+", "", guess)
      if (!is.null(guess) && guess != "" && !is.na(guess)) {
        if (((guess %in% answer_one) && option == question1) ||
          ((guess %in% answer_two) && option == question2)) {
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
        # Probability = c(1 / (2^n), 1 / (2^n - 1), 1 / 2^(n-1) )
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
