library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)

app1UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, 
             h1("Simpson's Paradox"),
             p("Simpson's Paradox is a statistical phenomenon where a
                trend that appears in different groups of data reverses
                or disappears when these groups are combined. This paradox
                illustrates the importance of considering underlying
                variables that might influence the results. It reveals how
                aggregated data can sometimes mislead us by concealing the
                true relationship between variables.
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
                
                This example highlights the importance of analyzing data in
                a segmented manner to avoid misleading conclusions and
                uncover the true nature of relationships within the data.
                The real data set consists of 85 departments, but we will
                only consider six since it is visually more appealing."
        )
      )
    ),
    fluidRow(
      column(6,
             box(
               title = "Admissions By Gender", 
               status = "primary", 
               solidHeader = TRUE, 
               collapsible = TRUE,
               collapsed = TRUE,
               width = NULL,
               plotOutput(ns("admissionsPlot"))
             ),
             box(
               title = "Admissions By Gender In Departments", 
               status = "primary", 
               solidHeader = TRUE, 
               collapsible = TRUE,
               collapsed = TRUE,
               width = NULL,
               plotOutput(ns("departmentPlot"))
             )
      ),
      column(6,
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
      )
    ),
    fluidRow(
      column(12, 
             h3("Cholesterol-Exercise example"),
             p(" When we plot exercise on the X-axis and cholesterol on the 
             Y-axis, and segregate by age (check 'Group by Age'), we observe a 
             general downward trend in each age group: the more young people 
             exercise, the lower their cholesterol levels, and this pattern 
             holds for middle-aged and elderly individuals as well. However, 
             if we create the same scatter plot without segregating by age 
             (uncheck 'Group by Age'), we see an overall upward trend: the more 
             a person exercises, the higher their cholesterol appears to be.

             To understand this discrepancy, we need to consider the underlying 
             data. Older individuals, who are more likely to engage in 
             exercise, are also more likely to have high cholesterol, regardless
             of their exercise habits. This means age is a common factor 
             influencing both exercise (treatment) and cholesterol levels 
             (outcome). Therefore, to make accurate comparisons, we should 
             analyze the age-segregated data. This approach allows us to compare
             individuals of the same age group, thereby eliminating the age 
             factor and clarifying that higher cholesterol in high exercisers 
             is due to their age, not their exercise routine.")
      )
    ),
    fluidRow(
      column(9,
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
      column(3,
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
    )
  )
}

app1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # read data files
    total_data <- read.csv("./data/total_admissions.csv")
    department_data <- read.csv("./data/department_admissions.csv")
    exercise_data <- read.csv("./data/exercise_data.csv")
    
    # round data points to one decimal place
    exercise_data <- exercise_data %>%
      mutate(Exercise = round(Exercise, 1),
             Cholesterol = round(Cholesterol, 1))
    
    output$admissionsPlot <- renderPlot({
      ggplot(total_data, aes(x = Gender, y = AdmissionRate, fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("blue", "red")) +
        labs(title = "Admissions By Gender", x = "Gender", 
             y = "Admission Rate") +
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
          plot.background = element_rect(fill = "#ffffff", color = NA, 
                                         size = 1, linetype = "solid"),
          panel.border = element_rect(fill = NA, color = "black", 
                                      size = 1, linetype = "solid")
        )
    })
    
    output$departmentPlot <- renderPlot({
      ggplot(department_data, aes(x = Department, y = AdmissionRate, 
                                  fill = Gender)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        scale_fill_manual(values = c("blue", "red")) +
        labs(title = "Admissions By Gender In Departments", x = "Department", 
             y = "Admission Rate") +
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
          plot.background = element_rect(fill = "#ffffff", color = NA, 
                                         size = 1, linetype = "solid"),
          panel.border = element_rect(fill = NA, color = "black", 
                                      size = 1, linetype = "solid")
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
        ggplot(exercise_data, aes(x = Exercise, y = Cholesterol, 
                                  color = factor(Avg.Age))) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
          scale_color_manual(values = c("red", "green", "blue", "purple", 
                                        "orange")) +
          labs(title = "Exercise vs. Cholesterol - Grouped by Age",
               x = "Exercise",
               y = "Cholesterol Level",
               color = "Age Group") +
          theme_minimal()
      } else {
        ggplot(exercise_data, aes(x = Exercise, y = Cholesterol)) +
          geom_point(color = "black") +
          geom_smooth(method = "lm", formula = y ~ x, color = "red", 
                      se = FALSE) +
          labs(title = "Exercise vs. Cholesterol - Aggregated Data",
               x = "Exercise",
               y = "Cholesterol Level") +
          theme_minimal()
      }
    })
    
    # Render the exercise data table
    output$exerciseDataTable <- renderDataTable({
      datatable(exercise_data)
    })
  })
}
