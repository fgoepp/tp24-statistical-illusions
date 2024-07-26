library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

app1UI <- function(id) {
  ns <- NS(id)
  tagList(
    withMathJax(),
    fluidRow(
      column(
        12,
        h1("Simpson's Paradox"),
        p("Simpson's Paradox is a statistical phenomenon where groups
                of data show a particular trend, but this trend is reversed
                when the groups are combined together. Understanding and
                identifying this paradox is important for correctly
                interpreting the data.
                A classic example of Simpson's Paradox occurred during the
                analysis of graduate admissions at the University of
                California, Berkeley in the 1970s. Initially, the overall
                data suggested a bias against female applicants, showing
                that men had a significantly higher acceptance rate than
                women."),
        p(" The real data set consists of 85 departments, but for 
            simplicity we only use six here.
          
            Consider the following two bar charts, on the left, the 
            admissions of the aggregated data for male and female and on
            the right, the admissions of females and males by department:")
      )
    ),
    fluidRow(
      column(
        6,
        box(
          title = "Main Setup: Admissions By Gender",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          plotOutput(ns("admissionsPlot"))
        ),
        box(
          title = "Admissions Data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          uiOutput(ns("dataTableTotal")),
          uiOutput(ns("dataTableDepartment"))
        )
      ),
      column(
        6,
        box(
          title = "Main Setup: Admissions By Gender In Departments",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          plotOutput(ns("departmentPlot")),
          p("When the admissions data was broken down by
         department, a different story emerged. Within each
         department, the acceptance rates for women were similar to
         or higher than those for men. The apparent bias was due to
         women applying to more competitive departments with lower
         overall acceptance rates, while men applied to departments
         with higher acceptance rates. To see all the absoulte values in
         detail, open the admissions data table. More women applied to
        departments D, E and F, which had lower acceptance rates, than the other
        departments. Hence, most of them got rejected, while less women applied to
        departmens A and B, of which most of the applicants got accepted. In
        contrast to that, the majority of men applied to departments A and B
        which had higher acceptance rates. Interesting, how lack of information
        can distort reality, isn't it?")
        )
      )
    ),
    fluidRow(
      column(
        9,
        box(
          title = "Mathematical Background",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          wellPanel(
            style = "background-color: #f5f5f5; padding: 15px;",
            p(HTML("(Recap) <b>Law of total probability</b>:
            If \\(\\Omega = K_1 \\biguplus K_2 \\biguplus ... \\biguplus K_n\\),
            then $$P(X) = \\sum_{i=1}^n P(K_i) \\cdot P(X|K_i) $$"))
          ),
          br(),
          p("For events \\(X, Y \\subset \\Omega \\text{ and }
          \\Omega = K_1 \\biguplus K_2 \\biguplus ... \\biguplus K_n \\)
          there holds:
          $$P(Y|X, K_i) > P(Y|\\neg X, K_i)  \\;\\;\\;\\;
          \\forall i =1, ..., n $$
          and $$ P(Y|X) < P(Y| \\neg X) $$"),
          br(),
          p("So for events
            \\(X\\) and \\(Y\\) in a sample space \\(\\Omega\\) partitioned into
            disjoint subsets \\(K_1, K_2, \\ldots, K_n\\), we have 
            \\(P(Y|X, K_i) > P(Y|\\neg X, K_i)\\) for all \\(i\\), indicating that
            within each subset, \\(X\\) increases the probability of \\(Y\\).
            But, when combining the data across all subsets, we find
            \\(P(Y|X) < P(Y|\\neg X)\\). This reversal occurs because the overall 
            probabilities \\(P(Y|X)\\) and \\(P(Y|\\neg X)\\) are weighted averages 
            of the probabilities within each subset (see Law of total
            probability), \\(P(Y|X, K_i)\\) and 
            \\(P(Y|\\neg X, K_i)\\), respectively. It is important
            to keep in mind, that this reversal CAN happen and does not
            happen every time. If it happens, we have Simpson's
            Paradox. The distribution of \\(X\\) and 
            \\(\\neg X\\) across the subsets can result in a combined probability 
            that contradicts the trends observed within each subgroup. Interesting
            , isn't it?"),
          br(),
          p(HTML("<strong><u>Vector Interpretation</u></strong>")),
          p(
            tags$img(
              src = "/simpsons_paradox/vector_interpretation.png",
              style = "display: block; margin-left: auto; margin-right: auto;",
              height = "25%", width = "30%",
              alt = "Vector interpretation"
            )
          ),
          br(),
          p("
            Simpson's paradox can be illustrated using a 2d vector space. The idea
            is that, for example, a success rate \\( \\frac{p}{q} \\) 
            (i.e., successes/attempts) can 
            be represented by a vector \\( \\vec{A} = (q, p) \\), with a slope 
            of \\( \\frac{p}{q} \\). A steeper vector indicates a higher success
            rate. When combining two rates \\( \\frac{p_1}{q_1} \\) and 
            \\( \\frac{p_2}{q_2} \\), the resulting rate can be represented by 
            the sum of the vectors \\( (q_1, p_1) \\) and \\( (q_2, p_2) \\). 
            According to the parallelogram rule, this sum is the vector 
            \\( (q_1 + q_2, p_1 + p_2) \\), with a slope of 
            \\( \\frac{p_1 + p_2}{q_1 + q_2} \\).

            What the paradox now says is, even if a vector \\( \\vec{L}_1 \\) 
            (in orange in the figure) has a smaller slope than another vector 
            \\( \\vec{B}_1 \\) (in blue), and \\( \\vec{L}_2 \\) has a smaller 
            slope than \\( \\vec{B}_2 \\), the sum of the two orange vectors 
            \\( \\vec{L}_1 + \\vec{L}_2 \\) can still have a larger slope than 
            the sum of the two blue vectors \\( \\vec{B}_1 + \\vec{B}_2 \\). 
            For this to happen, one of the orange vectors must have a greater 
            slope than one of the blue vectors (in this case, \\( \\vec{L}_2 \\)
            and \\( \\vec{B}_1 \\)), and these vectors will generally be longer 
            than the alternatively subscripted vectors, because of it dominating 
            the overall comparison. The length of a vector corresponds to the 
            size of a (sub-)group in the UC Berkeley example above.
            
            But what, if two vectors have the same length, so the groups
            are the same size? It's simple - then
            there is no reversal of pattern. This is because the relative 
            contributions to the combined slopes remain consistent, since all
            groups are of the same size.
            The length of the vectors plays an important role in determining 
            the overall effect. 
            When vectors are of equal length, mathematically speaking, we have:
              \\[
                q_{L1}^2 + p_{L1}^2 = q_{B1}^2 + p_{B1}^2
                \\]
            \\[
              q_{L2}^2 + p_{L2}^2 = q_{B2}^2 + p_{B2}^2
              \\]
            
            The combined lengths are also equal:
              \\[
                \\sqrt{(q_{L1} + q_{L2})^2 + (p_{L1} + p_{L2})^2} = 
                \\sqrt{(q_{B1} + q_{B2})^2 + (p_{B1} + p_{B2})^2}
                \\]
            
            Since the slopes of individual vectors remain proportionate 
            and their lengths are equal:
              \\[
                \\frac{p_{L1} + p_{L2}}{q_{L1} + q_{L2}} < \\frac{p_{B1} + p_{B2}}{q_{B1} + q_{B2}}
                \\]
            
            So, the sum of two vectors with smaller slopes will always 
            have a smaller slope than the sum of two vectors with larger slopes 
            if their lengths are the same, preventing a reversal.
            ")
        )
      ),
     column(
       3, 
       box(
         title = "Historical Background",
         status = "primary",
         solidHeader = TRUE,
         collapsible = TRUE,
         collapsed = TRUE,
         width = NULL,
         p(HTML("The paradox was named after British statistician Edward Simpson,
               who detailed the effect in his
               <a href=https://math.bme.hu/~marib/bsmeur/simpson.pdf>
               work in 1951</a>.
               However, the phenomenon had been noticed earlier, with instances
               documented by
               <a href=https://royalsocietypublishing.org/doi/10.1098/rsta.1899.0006>
               Karl Pearson (1899)</a> and
               <a href=https://zenodo.org/records/1431599>Udny Yule (1903)</a>,
               two other
               statisticians. Simpsons provided an example involving medical
               treatments, where he used a 2x2x2 contingency table, where
               patients were classified by their treatment status, gender and
               surival outcome. When the data is aggregated without considering
               gender, the treatment appears less effective, showing no clear
               benefit or even a harmful effect. However, when analyzed
               separately by gender, the treatment shows a clear benefit for
               both men and women."))
       )
     )
    ),

    fluidRow(
      column(
        12,
        box(
          title = "Secondary Setup: Exercise vs. Cholesterol - (Motivated by 
          Glymour et al.)",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          p("Let's consider another example and visual representation of
          Simpson's paradox. Say, you are a doctor, who wants to see how
          cholesterol level and amount of exercice (sports) correlate. 
          Therefore, you examine 1000 patients by checking their cholesterol
          level and asking how much sports they do (e.g. hours per week). 
          You plot the data and see the graph
          below. Rising cholesterol level, by exercising more? The data is
          correct, but you forgot to consider an important information for
          interpreting the data, that is to consider the age of your patients.
          Luckily, you also have the age of each patient in your dataset and 
          can view the same plot for segregation of the data into each age group 
          (check 'Group by Age'). We observe a general downtrend in each group:
          the more young people exercise, the lower their cholesterol levels, 
          and this pattern holds for middle-aged and elderly individuals 
          as well.
          To understand the discrepancy, consider this:
          Older individuals, who are more likely to engage in
          exercise, are also more likely to have high cholesterol, regardless
          of their exercise habits. This means age is a common factor
          influencing both exercise (treatment) and cholesterol levels
          (outcome) - a so called 'confounder'."),
          checkboxInput(ns("groupByAge"), "Group by Age", value = FALSE),
          plotOutput(ns("combinedPlot"), height = "600px")
        )
      )
    ),
    fluidRow(
      box(
        title = "References",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 4,
        p(HTML("<a href=https://web.cs.ucla.edu/~kaoru/primer-complete-2019.pdf>
               Glymour, M., Jewell, N. P. & Pearl, J. (2016). Causal Inference 
               In Statistics A Primer, Published by: Wiley.  </a>")),
        p(HTML("<a href=https://math.bme.hu/~marib/bsmeur/simpson.pdf>
               Simpson, E. H. (1951). Journal of the Royal Statistical Society.
               Series B (Methodological), Vol. 13, No. 2  (pp.238-241). 
               Published by: Wiley for the Royal Statistical Society </a>")),
        p(HTML("<a href=https://plato.stanford.edu/entries/paradox-simpson/> 
               Simpson’s Paradox (Stanford Encyclopedia of Philosophy). 
               (2021, March 24). 
              </a>")),
        p(HTML("<a href=https://setosa.io/simpsons/> Simpson’s Paradox. 
        (n.d.). </a>")),
        p(HTML("<a href=https://en.wikipedia.org/wiki/Simpson%27s_paradox>
               Wikipedia contributors. (2024, June 24). Simpson’s paradox. 
               Wikipedia. 
              </a>"))
      )
    )
  )
}

app1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # read data files
    total_data <- read.csv("./data/simpsons_paradox/total_admissions.csv")
    department_data <- read.csv("./data/simpsons_paradox/department_admissions.csv")
    exercise_data <- read.csv("./data/simpsons_paradox/exercise_data.csv")

    # round data points to one decimal place
    exercise_data <- exercise_data %>%
      mutate(
        Exercise = round(Exercise, 1),
        Cholesterol = round(Cholesterol, 1)
      )

    output$admissionsPlot <- renderPlot({
      ggplot(total_data, aes(x = Gender, y = AdmissionRate, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        geom_text(aes(label = Admitted), vjust = -0.5, position = position_dodge(width = 0.7)) +
        scale_fill_manual(values = c("red", "blue")) +
        labs(
          title = "Admissions By Gender", x = "Gender",
          y = "Admission Rate"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(
            fill = "#ffffff", color = NA,
            size = 1, linetype = "solid"
          ),
          panel.border = element_rect(
            fill = NA, color = "black",
            size = 1, linetype = "solid"
          )
        )
    })

    output$departmentPlot <- renderPlot({
      ggplot(department_data, aes(
        x = Department, y = AdmissionRate,
        fill = Gender
      )) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        geom_text(aes(label = Admitted), vjust = -0.5, position =
                    position_dodge(width = 0.7)) +
        scale_fill_manual(values = c("red", "blue")) +
        labs(
          title = "Admissions By Gender In Departments", x = "Department",
          y = "Admission Rate"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "top",
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(
            fill = "#ffffff", color = NA,
            size = 1, linetype = "solid"
          ),
          panel.border = element_rect(
            fill = NA, color = "black",
            size = 1, linetype = "solid"
          )
        )
    })

    output$dataTableTotal <- renderUI({
      tags$table(
        class = "table table-bordered",
        style = "width: 100%;",
        tags$thead(
          tags$tr(
            tags$th("Gender"),
            tags$th("Admission Rate"),
            tags$th("Applicants"),
            tags$th("Admitted")
          )
        ),
        tags$tbody(
          lapply(1:nrow(total_data), function(i) {
            tags$tr(
              tags$td(total_data$Gender[i]),
              tags$td(total_data$AdmissionRate[i]),
              tags$td(total_data$Applicants[i]),
              tags$td(total_data$Admitted[i])
            )
          })
        )
      )
    })

    output$dataTableDepartment <- renderUI({
      tags$table(
        class = "table table-bordered",
        style = "width: 100%;",
        tags$thead(
          tags$tr(
            tags$th("Department"),
            tags$th("Gender"),
            tags$th("Admission Rate"),
            tags$th("Applicants"),
            tags$th("Admitted")
          )
        ),
        tags$tbody(
          lapply(1:nrow(department_data), function(i) {
            tags$tr(
              tags$td(department_data$Department[i]),
              tags$td(department_data$Gender[i]),
              tags$td(department_data$AdmissionRate[i]),
              tags$td(department_data$Applicants[i]),
              tags$td(department_data$Admitted[i])
            )
          })
        )
      )
    })

    # regression plot
    output$combinedPlot <- renderPlot({
      if (input$groupByAge) {
        ggplot(exercise_data, aes(
          x = Exercise, y = Cholesterol,
          color = factor(Avg.Age)
        )) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
          scale_color_manual(values = c(
            "red", "green", "blue", "purple",
            "orange"
          )) +
          labs(
            title = "Exercise vs. Cholesterol - Grouped by Age",
            x = "Exercise",
            y = "Cholesterol Level",
            color = "Age Group"
          ) +
          theme(axis.text.x=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(),  
                axis.ticks.y=element_blank() 
          )
        
      } else {
        ggplot(exercise_data, aes(x = Exercise, y = Cholesterol)) +
          geom_point(color = "black") +
          geom_smooth(
            method = "lm", formula = y ~ x, color = "red",
            se = FALSE
          ) +
          labs(
            title = "Exercise vs. Cholesterol - Aggregated Data",
            x = "Exercise",
            y = "Cholesterol Level"
          ) +
          theme(axis.text.x=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.text.y=element_blank(),  
                axis.ticks.y=element_blank()
          )
        
      }
    })

    # Render the exercise data table
    output$exerciseDataTable <- renderDataTable({
      datatable(exercise_data)
    })
  })
}
