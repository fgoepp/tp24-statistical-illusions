library(shiny)
library(ggplot2)
library(dplyr)
library(DT)


app1UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .table-bordered th, .table-bordered td {
          border: 1px solid black !important;
        }
        .card {
          border: 1px solid #dddddd;
          border-radius: 15px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
          padding: 15px;
          margin-bottom: 20px;
          background-color: #ffffff;
        }
      "))
    ),
    fluidRow(
      column(12, 
             h3("Simpson's Paradox"),
             p("Simpson's Paradox occurs when a trend that appears in different 
        groups of data disappears or reverses when these groups are combined. 
        In this example, aggregated data might show one trend, while the 
        disaggregated data by department shows a different trend. 
        This phenomenon can obscure the real relationships between variables.")
      )
    ),
    fluidRow(
      column(12, 
             checkboxInput(ns("showDepartments"), 
                           "Show Admissions by Department", FALSE)
      )
    ),
    fluidRow(
      column(6,
             div(class = "card",
                 plotOutput(ns("admissionsPlot"))
             ),
             div(class = "card", 
                 conditionalPanel(
                   condition = paste0("input['", ns("showDepartments"), 
                                      "'] == true"),
                   plotOutput(ns("departmentPlot"))
                 )
             )
      ),
      column(6,
             div(class = "card", 
                 h4("Admissions By Gender"),
                 uiOutput(ns("dataTableTotal"))
             ),
             div(class = "card", 
                 conditionalPanel(
                   condition = paste0("input['", ns("showDepartments"), 
                                      "'] == true"),
                   h4("Admissions By Gender In Departments"),
                   uiOutput(ns("dataTableDepartment"))
                 )
             )
      )
    )
  )
}

app1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    #  path to data  FIX ABSOULUTE PATH
    total_data_path <- "data/total_admissions.csv"
    department_data_path <- "data/department_admissions.csv"
    
    # check if files exist before reading
    if (!file.exists(total_data_path)) {
      stop("File not found: ", total_data_path)
    }
    if (!file.exists(department_data_path)) {
      stop("File not found: ", department_data_path)
    }
    
    total_data <- read.csv(total_data_path)
    department_data <- read.csv(department_data_path)
    
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
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "#f0f0f0", color = NA, 
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
          plot.background = element_rect(fill = "#f0f0f0", color = NA, 
                                         size = 1, linetype = "solid"),
          panel.border = element_rect(fill = NA, color = "black", 
                                      size = 1, linetype = "solid")
        )
    })
    
    output$dataTableTotal <- renderUI({
      tags$table(
        class = "table table-bordered",
        tags$thead(
          tags$tr(
            tags$th("Gender"),
            tags$th("Admission Rate")
          )
        ),
        tags$tbody(
          lapply(1:nrow(total_data), function(i) {
            tags$tr(
              tags$td(total_data$Gender[i]),
              tags$td(total_data$AdmissionRate[i])
            )
          })
        )
      )
    })
    
    output$dataTableDepartment <- renderUI({
      tags$table(
        class = "table table-bordered",
        tags$thead(
          tags$tr(
            tags$th("Department"),
            tags$th("Gender"),
            tags$th("Admission Rate")
          )
        ),
        tags$tbody(
          lapply(1:nrow(department_data), function(i) {
            tags$tr(
              tags$td(department_data$Department[i]),
              tags$td(department_data$Gender[i]),
              tags$td(department_data$AdmissionRate[i])
            )
          })
        )
      )
    })
  })
}
