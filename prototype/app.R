library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinyBS)
library(plotly)
library(dplyr)

# source module files
source("R/home_module.R")
source("R/app1_module.R")
source("R/app2_module.R")
source("R/app3_module.R")
source("R/app4_module.R")

# main UI
ui <- dashboardPage(
  dashboardHeader(title = "Shiny Apps Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("App 1", tabName = "app1", icon = icon("bar-chart")),
      menuItem("App 2", tabName = "app2", icon = icon("line-chart")),
      menuItem("App 3", tabName = "app3", icon = icon("pie-chart")),
      menuItem("St. Petersburg Paradox", tabName = "app4", 
               icon = icon("piggy-bank"))
    )
  ),
  dashboardBody(
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
