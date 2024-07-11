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
        h1("Simpson's Paradox - when facts aren't facts"),
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
        p(" However, when the admissions data was broken down by
                department, a different story emerged. Within each
                department, the acceptance rates for women were similar to
                or higher than those for men. The apparent bias was due to
                women applying to more competitive departments with lower
                overall acceptance rates, while men applied to departments
                with higher acceptance rates.

                The real data set consists of 85 departments, but we will
                only consider six since it is visually more appealing.")
      )
    ),
    fluidRow(
      column(
        6,
        box(
          title = "Admissions By Gender",
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
          title = "Admissions By Gender In Departments",
          status = "primary",
          solidHeader = TRUE,
          width = NULL,
          plotOutput(ns("departmentPlot"))
        )
      )
    ),
    fluidRow(
      box(
        title = "Historical background",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12,
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
    ),
    fluidRow(
      box(
        title = "Mathematical definition",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        p(HTML("Recap <b>Law of total probability</b>:
          If \\(\\Omega = K_1 \\biguplus K_2 \\biguplus ... \\biguplus K_n\\),
          then \\(P(X) = \\sum_{i=1}^n P(K_i) \\cdot P(X|K_i) \\)")),
        p("

          "),
        p("For events \\(X, Y \\subset \\Omega \\text{ and }
          \\Omega = K_1 \\biguplus K_2 \\biguplus ... \\biguplus K_n \\)
          there holds:
          $$P(Y|X, K_i) > P(Y|\\neg X, K_i)  \\;\\;\\;\\;
          \\forall i =1, ..., n $$
          and $$ P(Y|X) < P(Y| \\neg X) $$")
      )
    ),
    fluidRow(
      box(
        title = "Cholesterol-Exercise example (Motivated by Glymour et al.)",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        p(" When we plot exercise on the X-axis and cholesterol on the
             Y-axis, and segregate by age (check 'Group by Age'), we observe a
             general downward trend in each age group: the more young people
             exercise, the lower their cholesterol levels, and this pattern
             holds for middle-aged and elderly individuals as well. However,
             if we create the same scatter plot without segregating by age
             (uncheck 'Group by Age'), we see an overall upward trend: the more
             a person exercises, the higher their cholesterol appears to be.

             To understand this difference, we need to consider the underlying
             data. Older individuals, who are more likely to engage in
             exercise, are also more likely to have high cholesterol, regardless
             of their exercise habits. This means age is a common factor
             influencing both exercise (treatment) and cholesterol levels
             (outcome). Therefore, to make accurate comparisons, one should
             analyze the age-segregated data.")
      )
    ),
    fluidRow(
      column(
        9,
        box(
          title = "Exercise vs. Cholesterol - Simpson's Paradox",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          checkboxInput(ns("groupByAge"), "Group by Age", value = FALSE),
          plotOutput(ns("combinedPlot"), height = "600px")
        )
      ),
      column(
        3,
        box(
          title = "Exercise Data",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          collapsed = TRUE,
          width = NULL,
          dataTableOutput(ns("exerciseDataTable"))
        )
      )
    ),
    fluidRow(
      box(
        title = "References and further reading",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        width = 4,
        p(HTML("<a href=https://math.bme.hu/~marib/bsmeur/simpson.pdf>
               Original paper by Simpson</a>")),
        p(HTML("<a href=https://web.cs.ucla.edu/~kaoru/primer-complete-2019.pdf>Glymour et al. </a>")),
        p(HTML("<a href=https://setosa.io/simpsons/>Setosa</a>")),
        p(HTML("<a href=https://en.wikipedia.org/wiki/Simpson%27s_paradox>
               Wikipedia</a>")),
        p(HTML("<a href=https://plato.stanford.edu/entries/paradox-simpson/>
               Stanford Encyclopedia of Philosophy </a>"))
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
          theme_minimal()
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
          theme_minimal()
      }
    })

    # Render the exercise data table
    output$exerciseDataTable <- renderDataTable({
      datatable(exercise_data)
    })
  })
}
