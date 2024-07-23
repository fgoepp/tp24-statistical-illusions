library(shiny)
library(bslib)
library(plotly)
library(shinyBS)


question1 <- "QUESTION1: At least one of the two children is a boy.
What is the probability that both children are boys?"
question2 <- "QUESTION2: The older child is a boy. What is the probability that
both children are boys?"
question3 <- "QUESTION3: At least one of the two children is a boy and born on
a Tuesday. What is the probability that both children are boys?"
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
      "Imagine you're having a nice conversation with your neighbor Mr. Smith. He
      tells you that he has two children. The two questions then are the following:"
    ),
    p(
      HTML("<i>1. if (at least) one of the two children is a boy. What is the probability
      that both children are boys?</i> <br>
           <i>2. if the older child is a boy. What is the probability that both
           children are boys?</i>")
    ),
    p(
      "In the following section, you can try solving these problems yourself,
      view visual representations, and learn their solutions. Throughout this
      app, you can assume that the sex of the two children is independent of one
      another and equally likely to be a boy or a girl."
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
            is in the details. You can try solving the questions yourself:",
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
          )
          # fluidRow(
          #   column(
          #     12,
          #     p("
          #       Hopefully, you've already clicked the 'Show Answer' button and
          #       seen the conflicting views regarding the two questions. This is
          #       exactly the first lesson we can learn from the Two Children Problem:
          #       the fact that when formulating a problem, we have to make sure that
          #       we are as exact and clear as possible. Even the smallest room for
          #       interpretation can lead to the problem having vastly different answers.
          #       This highlights the importance of ",
          #       tags$span("clarity, accuracy, and interpretation ",
          #         style = "color: purple; font-weight: bold;"
          #       ),
          #       "in probability and math.",
          #       style = "margin-top: 20px; margin-left: 20px;"
          #     ),
          #     p(
          #       "Now, just to be clear, we assume the pure mathematical meaning
          #       of the phrase 'one of them,' meaning that the expression equals
          #       'at least one of them.' and consider the probability of a child
          #       being born a male or a female to be equally likely. Additionally
          #       , using our mathematical approach, because it isn't explicitly
          #       mentioned in Question 1 that we have seen the child who is a boy,
          #       we assume that it truly is by chance that at least one of them
          #       is a boy.",
          #       style = "margin-left: 20px;"
          #     )
          #   )
          # )
        )
      )
    ),
    # =========== BOX NR. 2 ===========
    fluidRow(
      box(
        width = 8, title = "Mathematical Background", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        p("In this section we will take a look at the math behind the two-children
          problem and its variation and how their probabilities can be derived.
          These derived formulas will also be used in
          the 'Graphs And Plots' section to plot everything(please look at the other sections first
          to familiarize yourself with all the questions mentioned. This section
          should be visited last as it relies on the other ones)."),
        p(HTML
        ("The four questions are listed below: <br>
               <i>
                <b style='color:red;'>*</b>
                If a family has N children, what is the probability that
                all children are boys? <br>

                <b style='color:green;'>**</b> If a family has N children
                and at least one of them is a boy,  What is the probability
                that all children are boys? <br>

                <b style='color:brown;'>***</b> If a family has N children and
                the oldest child is a boy, What is the probability that
                all children are boys? <br>

                <b style='color:violet;'>****</b> If a family has N children and
                at least one of them is a boy born on Tuesday, What is the
                probability that all children are boys?
              </i> <br>

               <h4 style='color:blue;'> When N = 2: </h4>

                <b><i style='color:red;'>#</i></b> In this case we have 4 valid
                combinations: BB, BG, GB, GG (B = boy and G = girl) and we only
                want one combination (BB) \\(\\rightarrow \\) \\( P (\\text{2 boys})
                = \\frac{1}{4} = 0.25\\)
               <br>

                <b><i style='color:green;'>##</i></b> In this case we have 3 valid combinations:
               BB, BG, GB and we only want one combination (BB) \\(\\rightarrow\\)
               \\(
               P (\\text{2 boys} \\mid \\text{at least 1 child is a boy})
               = \\frac{1}{3} \\approx 0.33 \\)
               <br>

                <b><i style='color:brown;'>###</i></b> In this case we have 2 valid combinations:
               BB, BG and we only want one
               combination (BB) \\(\\rightarrow \\) \\( P (\\text{2 boys} \\mid
               \\text{oldest child is a boy}) =
               \\frac{1}{2} = 0.5 \\)
               <br>

                <b><i style='color:violet;'>####</i></b> We are going to do things a bit differently and
               calculate an even more general form of this case. Suppose that
               the chance of being born on any given day of the week is
               \\(\\frac{1}{7} \\). From Bayes' Theorem that the probability of
               two boys \\(BB\\), given that one boy was born on a Tuesday
               (\\(B_T\\)) is given by: <br>
               \\( P (BB \\mid B_T) = \\frac{P (BB \\mid B_T) \\cdot P(BB)}{P(B_T)}
               \\) <br>
               Assume that the probability of being born on a Tuesday is \\(
               \\varepsilon = \\frac{1}{7}\\) which will be set after arriving
               at the general solution. The second factor in the numerator is simply
               \\(\\frac{1}{4}\\), the probability of having two boys. The first
               term in the numerator is the probability of at least one boy born
               on Tuesday, given that the family has two boys, or
               \\(1 - (1-\\varepsilon)^2\\) (one minus the probability that
               neither boy is born on Tuesday). For the denominator, let us
               decompose: \\(P(B_{T})=P(B_{T}\\mid BB)P(BB)+P(B_{T}\\mid BG)P(BG)+
               P(B_{T}\\mid GB)P(GB)+P(B_{T}\\mid GG)P(GG)\\). Each term is
               weighted with probability \\(\\frac{1}{4}\\). The first term is
               already known by the previous remark, the last term is 0 (there
               are no boys). The term \\(P(B_{T} \\mid BG)\\) and
               \\(P(B_{T} \\mid GB)\\) is \\(\\varepsilon\\), there is one and
               only one boy, thus he has \\(\\varepsilon\\) chance of being born
               on Tuesday. Therefore, the full equation is: <br>
               \\(P(BB \\mid B_{T}) = \\frac{\\left(1 -
               (1 - \\varepsilon)^{2}\\right) \\times \\frac{1}{4}}{0 +
               \\frac{1}{4} \\varepsilon + \\frac{1}{4} \\varepsilon +
               \\frac{1}{4} \\left(\\varepsilon + \\varepsilon -
               \\varepsilon^{2}\\right)} = \\frac{1 - (1 - \\varepsilon)^{2}}{4
               \\varepsilon - \\varepsilon^{2}}\\). <br>
               For \\(\\varepsilon > 0\\), this reduces to \\(P(BB \\mid B_{T})
               = \\frac{2 - \\varepsilon}{4 - \\varepsilon}\\). If \\(\\varepsilon\\)
               is now set to \\(\\frac{1}{7}\\), the probability becomes
               \\(\\frac{13}{27}\\), or about 0.48. In fact, as \\(\\varepsilon\\)
               approaches 0, the total probability goes to \\(\\frac{1}{2}\\),
               which is the answer expected when one child is sampled
               (e.g., the oldest child is a boy) and is thus removed from the
               pool of possible children. In other words, as more and more
               details about the boy child are given (for instance: born on
               January 1), the chance that the other child is a girl approaches
               one half.
               <br>

               <h4 style='color:blue;' > When N = n \\(\\geq\\) 2: </h4>

                <b><i style='color:red;'>#</i></b> In this case we have n children which all have 2
               possibilities (being a girl or a boy), which gives us a total
               \\(\\ 2^n \\) valid combinations of which we only need 1 (all boys)
               \\(\\rightarrow \\) \\( P (\\text{n boys}) =
               \\frac{1}{2^n}\\)
               <br>

                <b><i style='color:green;'>##</i></b> In this case we would have also had
               \\(\\ 2^n \\) combinations but because we know that at least one
               of the n children is a boy our valid combinations are \\(\\ 2^n - 1\\)
              (the 1 is the case where all children are girls). From these combinations
              we only need 1 (all boys) \\(\\rightarrow \\) \\( P (\\text{n boys}
               \\mid \\text{at least one child is a boy}) = \\frac{1}{2^n - 1}\\)
               <br>

                <b><i style='color:brown;'>###</i></b> In this case because
                we know that the oldest
                child is a boy our valid combinations are \\(\\ 2^{n-1}\\)
                (the n-1 is because n-1 children can be either a boy or a girl but
                1 child, which is the oldest child is a boy). From these combinations
                we only need 1 (all boys) \\(\\rightarrow \\) \\( P (\\text{n boys}
                \\mid \\text{the oldest child is a boy}) = \\frac{1}{2^{n - 1}}
                \\)
               <br>

                <b><i style='color:violet;'>####</i></b> In this case, we treat
                (gender + 7 days of
                the week) as one entity in order to make it easier to solve this
                problem. First, let's take a look at our sought-out combinations:<br>
                \\( n(\\text{all children being boys} \\mid \\text{at least one boy
                born on Tuesday}) \\) which
                we can calculate via its complement:
                \\(n(\\text{all children being boys}) - n(\\text{all children being boys}
                \\mid \\text{no boy born on Tuesday})\\) = \\(7^n - 6^n\\).
                In the first term, we have 7 because we're looking for 1 gender (boy)
                and 7 days: \\(1 \\cdot 7\\). In the second term, we have 1 gender
                and 6 days (no Tuesday): \\(1 \\cdot 6\\).<br>
                Now, let's take a look at all combinations where there is at
                least one boy born on a Tuesday. Again, let's use the complement:
                \\(n(\\text{all children})- n(\\text{all children} \\mid \\text{no boy is born a Tuesday})\\)
                = \\(14^n - 13^n\\).
                In the first term, we have 14 because we're considering two
                genders and 7 days: \\(2 \\cdot 7\\). In the second term,
                we have 13 because we're considering no boys born on Tuesday,
                meaning that girls and 7 days (7 possibilities) and boys and 6 days
                (6 possibilities) together: \\(1 \\cdot 7 + 1 \\cdot 6 = 13\\).
                In total, the desired combination over the entire combinations
                gives us the probability of:
                \\(P(\\text{n boys} \\mid
                \\text{at least one child is a boy born on a Tuesday})
                = \\frac{7^n - 6^n}{14^n - 13^n}\\)<br>

               "))
      ),
      box(
        width = 4, title = "Historical background", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        p(
          tags$p("The two children problem, also known as the boy or girl paradox,
                 is a well-known problem in probability theory. Its initial formulation
                 dates back to October 1959, when it was published by Martin Gardner
                 in the 'Mathematical Games' column in Scientific American,
                 a popular American science magazine."),
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
      )
    ),
    # =========== BOX NR. 3 ===========
    fluidRow(
      box(
        width = 12, title = "Secondary Setup: The Tuesdayboy Variation", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        p(HTML("The Tuesday Boy Problem is an intriguing variation and extension
        of the classic Two Children Problem. It introduces a third question to
        our setup, which is: <br>
        Just like before, you are speaking with Mr. Smith, who tells you he has
        two children. The new question is: <br> <br>
        <i>if at least one of the two children is a boy and born on a Tuesday,
        What is the probability that both children are boys?</i> <br>")),
        checkboxInput(ns("show_gif"), "show GIF (uncheck to see still image)", TRUE),
        uiOutput(ns("gif_ui")),
        column(
          12,
          p("The illustration above helps showcase the possible valid combinations
            (where at least one boy is born on a Tuesday) using blue squares (27).
            Among these, the combinations we are interested in (where both children
            are boys) are highlighted with green squares (13). This gives us
            a probability of 13/27 \\(\\approx 0.48\\)",
            style = "margin-top: 10px;"
          ),
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
    # =========== BOX NR. 4 ===========
    fluidRow(
      box(
        width = 12, title = "Graphs And Plots", status = "primary",
        collapsible = TRUE, collapsed = TRUE,
        solidHeader = TRUE,
        fluidRow(
          column(
            12,
            p(
              "In this section, in the left plot, we will examine the previous
              three questions, plus an additional new question, but this time
              not just for 2 children but rather for ",
              tags$span("n children", style = "color: purple; font-weight: bold;"),
              ". In the right plot We will be generalizing the Tuesday
              boy problem for 2 children."
            ),
            p(HTML
            ("The questions are listed again below (with the first being the
              newly added variation): <br> <i>
              _ Mr. Smith has n children. What's the probability that all n children
              are boys? <br>
              _ Mr. Smith has n children, and at least one of them is a boy. What's
              the probability that all n children are boys? (Question 1)<br>
              _ Mr. Smith has n children, and at least one of them is a boy and
              born on a Tuesday. What's the probability that all n children
              are boys? (Tuesday boy)<br>
              _ Mr. Smith has n children, and the oldest child is a boy. What's
              the probability that all n children are boys? (Question 2) </i><br>
              The more generalized version of the Tuesday boy problem (look at
              'Mathematical Background' for more in-depth explanation):<br><i>
              _ Mr. Smith has 2 children, and at least one of them is a boy and
              <span style='color:red; font-weight:bold;'>
              [some condition/information]</span>.
              What's the probability that all 2 children are boys?</i>")),
            column(9,
              p(HTML("Use the slider to change the number of children we
              have and play around with the probabilities.<br> Notice that
              that there's an increase of probability among the four question
              (from the question with least information to the question with most
              information about the children)
              which doesn't change with the number of children!")),
              style = "margin-top:30px;"
            ),
            column(3,
              p(
                "Come up with your own extra", tags$span("[condition/information]",
                  style = "color:red; font-weight:bold;"
                ), "for the generalized version of the Tuesday boy problem! 
                for example 'is born on 1. January' in the first input and 
                its corresponding probability '1/365' in the second one.",
                tags$span(
                  "Hover over for a few more examples",
                  `data-toggle` = "tooltip",
                  `data-placement` = "top",
                  title = HTML("Examples:<br>
              - 'is born on Christmas', 2/365<br>
              - 'is born on a Tuesday', 1/7<br>
              - 'is Getting audited by the IRS', 0.2 (according to Google)"),
                  style = "color:red; cursor: pointer;"
                )
              ),
              style = "margin-top:30px;",
              tags$script(HTML(
                "$(function () {
                $('[data-toggle=\"tooltip\"]').tooltip({ html: true });});"
                )
              )
            )
          ),
          column(
            12,
            column(9,
              sliderInput(ns("n_children"), "Number of children:",
                min = 2,
                max = 10, value = 2
              ),
              style = " margin-top:15px;"
            ),
            column(3,
              textInput(
                inputId = ns("arbitrary_discription"),
                label = "Enter your additional condition/info.",
                placeholder = "Enter a text string"
              ),
              textInput(
                inputId = ns("arbitrary_probability"),
                label = "Enter the probability of your condition/info.",
                placeholder = "Enter a fraction or decimal number"
              ),
              style = "margin-top:30px;"
            )
          ),
          column(
            12,
            column(
              9, plotlyOutput(ns("bar_plot"))
            ),
            column(3, plotlyOutput(ns("single_bar_plot"), width = 250))
          )
        )
      )
    ),
    # =========== BOX NR. 5 ===========
    fluidRow(
      box(
        width = 6, title = "References", status = "primary",
        collapsible = TRUE, collapsed = TRUE, solidHeader = TRUE,
        fluidRow(column(
          10,
          p(
            "Boy or girl paradox. (n.d.). In Wikipedia. ",
            a("https://en.wikipedia.org/wiki/Boy_or_girl_paradox",
              href = "https://en.wikipedia.org/wiki/Boy_or_girl_paradox"
            )
          ),
          p(
            "Kevin Olding - Mathsaurus. (2023, August 22). ",
            a("This Probability Puzzle Will Break Your Intuition |
              The Tuesday Boy Problem",
              href = "https://www.youtube.com/watch?v=90tEko9VFfU"
            ),
            ". YouTube."
          ),
          p(
            "ThePuzzlr. (2020, July 26). ",
            a("The Boy Or Girl Paradox",
              href = "https://www.youtube.com/watch?v=YtK4R66_YAk"
            ),
            ". YouTube."
          ),
          p(
            "OpenAI. (2024). (ChatGPT v3.5 & v4). ",
            a("https://www.openai.com/chatgpt",
              href = "https://www.openai.com/chatgpt"
            )
          )
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

    # =========== display answers ===========
    observeEvent(input$show_answer, {
      option <- input$dropdown_menu
      if (!is.null(option) && !is.na(option)) {
        if (option == question1) {
          showModal(modalDialog(
            title = "Answer",
            p(HTML("<h2> 1/3 (&#8776; 0.33)</h2> <br>
            But why? Now, if you used the visual hint, you saw the valid combinations
                   for two children. We have, in total, three valid combinations:
                   BB, BG, GB (B = boy and G = girl),
                   of which we want only one combination (BB), giving us a probability
                   of 1/3.")),
            p("but wait a lot of people have argued that the probability
            is actually 1/2. if we assume that the phrase 'at least
              one of them is a boy' hints that during our conversation, we have
              actually seen or at least know which of the two children is a boy,
              then the change of the other child being a boy is coin toss or 1/2!
              in some variations this first question is phrased as '(at least) one
              of them is a boy. What is the probability that both children are boys?'
              in which case some could even say that in a day to day conversation,
              if we are told that Mr. Smith has two kids and one of them is a boy,
              well then the other one is a girl! obviously if the other one was a boy
              as well then they would have simply said that 'they have two boys'
              from the get go. So therefor the probability that Mr.Smith has
              two boys is zero!!"),
            p("This highlights an important lesson from the Two Children Problem:
              the importance of being exact and clear in problem formulation.
              Even minor ambiguities can lead to vastly different answers,
              emphasizing the need for clarity and accuracy in probability
              and math. In this question we assume 'one of them' means
              'at least one of them' and since it's not explicitly stated that
              we have seen the boy, we assume it is by chance that at least
              one child is a boy.")
          ))
        } else if (option == question2) {
          showModal(modalDialog(
            title = "Answer",
            p(
              HTML("<h2> 1/2 (0.5)</h2> <br>
              But why? if you used the visual hint, you saw the valid combinations
                   for two children. We have, in total, two valid combinations:
                   BB, BG (B = boy and G = girl),
                   of which we want only one combination (BB), giving us a probability
                   of 1/2."),
              p("but wait in this case some people could say that in a day to day
            conversation, if we are told that Mr. Smith has two kids and one of
            them is a boy, well then the other one is a girl! obviously if the
            other one was a boy as well then they would have simply said that
              'they have two boys' from the get go. So therefor the probability
              that Mr. Smith has two boys is zero!!"),
              p("This highlights an important lesson from the Two Children Problem:
              the importance of being exact and clear in problem formulation.
              Even minor ambiguities can lead to vastly different answers,
              emphasizing the need for clarity and accuracy in probability
              and math.")
            )
          ))
        }
      }
    })

    # =========== check answers ===========
    observeEvent(input$submit_guess, {
      option <- input$dropdown_menu
      answer_one <- list("1/3", "0.33", "0.3", "0.333")
      answer_two <- list("1/2", "0.5")
      guess <- input$guess_probability
      guess <- gsub("\\s+", "", guess) # remove white spaces
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

    # =========== bar_plot ===========
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

    # =========== single_bar_plot ===========

    plot_data_single <- reactive({
      d <- input$arbitrary_discription
      p <- input$arbitrary_probability
      p <- gsub("\\s+", "", p) # remove white spaces

      prob <- tryCatch(
        { # evaluate probability into numeric
          eval(parse(text = p))
        },
        error = function(e) {
          0 # Return 0 if there is an error in evaluation
        }
      )

      if (!is.numeric(prob) || is.na(prob) || prob < 0 || prob > 1) {
        prob <- 0
      } else {
        # Calculate and round the adjusted probability
        prob <- tryCatch(
          {
            # Ensure the denominator is not zero
            round((2 - prob) / (4 - prob), 6)
          },
          error = function(e) {
            0
          }
        )
      }


      data.frame(
        Case = d,
        Probability = prob
      )
    })


    output$single_bar_plot <- renderPlotly({
      data <- plot_data_single()

      # Generate a vertical bar plot
      plot_ly(data,
        x = ~ paste("at least one of them is boy and...<br>", Case),
        y = ~Probability,
        type = "bar",
        orientation = "v", # Vertical bar
        marker = list(color = "darkblue"),
        text = ~ paste("Probability:<br>", Probability),
        hovertext = ~ paste("Probability:", Probability),
        hoverinfo = "text",
        showlegend = FALSE
      ) %>%
        layout(
          yaxis = list(
            title = "Probability",
            range = c(0, 1),
            tickvals = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
            ticktext = c("0", "0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1"),
            zeroline = FALSE,
            titlefont = list(size = 16),
            tickfont = list(size = 14)
          ),
          xaxis = list(
            title = "2 children",
            zeroline = FALSE,
            showgrid = FALSE,
            showticklabels = TRUE
          ),
          paper_bgcolor = "white",
          plot_bgcolor = "white",
          bargap = 0.5,
          transition = list(duration = 500, easing = "cubic-in-out")
        ) %>%
        config(displayModeBar = FALSE)
    })
  })
}
