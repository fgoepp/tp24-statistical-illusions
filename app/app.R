library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(plotly)
library(dplyr)
library(styler)

# source module files
source("modules/home_module.R")
source("modules/app1_module.R") # simpsons paradox
source("modules/app2_module.R") # monty hall problem
source("modules/app3_module.R") # the two child problem
source("modules/app4_module.R") # st. petersburg paradox

# main UI
ui <- dashboardPage(
  dashboardHeader(title = "Statistical Illusions"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Simpson's Paradox", tabName = "app1", icon = icon("bar-chart")),
      menuItem("Monty Hall Problem", tabName = "app2", 
               icon = icon("line-chart")),
      menuItem("Two Children Problem", tabName = "app3", icon = 
                 icon("pie-chart")),
      menuItem("St. Petersburg Paradox", tabName = "app4", 
               icon = icon("piggy-bank"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", 
                href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/
                5.15.4/css/all.min.css")
    ),
    tabItems(
      tabItem(tabName = "home", homeUI("home")),
      tabItem(tabName = "app1", app1UI("app1")),
      tabItem(tabName = "app2", app2UI("app2")),
      tabItem(tabName = "app3", app3UI("app3")),
      tabItem(tabName = "app4", app4UI("app4"))
    )
  )
)

# main Server
server <- function(input, output, session) {
  homeServer("home")
  app1Server("app1")
  app2Server("app2")
  app3Server("app3")
  app4Server("app4")
}

shinyApp(ui, server)
