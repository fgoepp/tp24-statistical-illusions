library(shiny)
library(shinydashboard)

# Load module code
source("monty_hall_module.R")
source("two_child_module.R")

# Define UI for landing page
landing_ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "https://fonts.googleapis.com/css2?family=DauphinPlain")
  ),
  
  tags$style(
    HTML("
         body {
           background-color: #f0f8ff; /* light blue */
         }
         
         .fade-in {
           opacity: 0;
           animation: fadeInAnimation 2s ease-in forwards;
         }
         
         @keyframes fadeInAnimation {
           0% { opacity: 0; }
           25% { opacity: 0.25; }
           50% { opacity: 0.5; }
           75% { opacity: 0.75; }
           100% { opacity: 1; }
         }
         
         h2 {
           font-family: 'DauphinPlain', sans-serif;
           margin-bottom: 20px;
           margin-top: 50px;
         }
         
         .dummy-text {
           margin-top: 100px;
           margin-left: 100px;
           margin-right: 100px;
           margin-bottom: 20px;
         }
         
         .illusion-text {
           margin-left: 100px;
           margin-right: 100px;
           margin-top: 100px;
           margin-bottom: 20px;
         }
         
         .action-btn {
           margin-top: 20px;
         }
         ")
  ),
  
  tags$div(
    style = "display: flex; justify-content: center; align-items: center; height: 100vh;",
    class = "fade-in",
    fluidRow(
      column(12,
             div(class = "fade-in text-center", 
                 h2("Hello, Welcome to my app")
             ),
             div(class = "fade-in text-center dummy-text", 
                 p("Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum.")
             ),
             div(class = "fade-in text-center illusion-text", 
                 p("Which illusion would you like to see?")
             ),
             div(class = "fade-in text-center action-btn", 
                 actionButton("monty_hall_btn", "Monty Hall Problem")
             ),
             div(class = "fade-in text-center action-btn", 
                 actionButton("two_child_btn", "Two Child Problem")
             ),
             div(class = "fade-in text-center action-btn", 
                 actionButton("other_btn", "Other Problem")
             )
      )
    )
  )
)

# Define UI for main dashboard
dashboard_ui <- dashboardPage(
  dashboardHeader(title = "Statistical Illusions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Monty Hall", tabName = "monty_hall"),
      menuItem("Two Child Problem", tabName = "two_child_problem"),
      menuItem("Other Problem", tabName = "other_problem")
    )
  ),
  dashboardBody(
    tabsetPanel(
      # Home tab
      tabPanel("Home",
               fluidRow(
                 column(12, 
                        h2("Welcome to Statistical Illusions!"),
                        p("This app allows you to explore various statistical illusions through interactive simulations.")
                 )
               )
      ),
      
      # Monty Hall tab
      tabPanel("Monty Hall",
               monty_hall_ui("monty_hall_module")  # Call Monty Hall module UI function
      ),
      
      # Two Child Problem tab
      tabPanel("Two Child Problem",
               two_child_problem_ui("two_child_problem_module")  # Call Two Child Problem module UI function
      ),
      
      # Other Problem tab
      tabPanel("Other Problem",
               # UI content for other problem tab
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize UI with landing page
  output$ui <- renderUI({
    landing_ui
  })
  
  # Observe event for button clicks on landing page
  observeEvent(input$monty_hall_btn, {
    output$ui <- renderUI({
      dashboard_ui
    })
  })
  
  observeEvent(input$two_child_btn, {
    output$ui <- renderUI({
      dashboard_ui
    })
  })
  
  observeEvent(input$other_btn, {
    output$ui <- renderUI({
      dashboard_ui
    })
  })
  
  # Call Monty Hall module server logic
  callModule(monty_hall_server, "monty_hall_module")
  
  # Call Two Child Problem module server logic
  callModule(two_child_problem_server, "two_child_problem_module")
}

# Run the application
shinyApp(ui = uiOutput("ui"), server = server)
