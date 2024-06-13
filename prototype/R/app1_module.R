# app1_module.R

app1UI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Simpson's Paradox"),
    p("This app demonstrates Simpson's Paradox using SAT scores and 
      teacher salaries."),
    fluidRow(
      box(width = 12, title = "Controls", status = "primary", 
          solidHeader = TRUE, sliderInput(ns("integer"), 
          "Adjust Salaries and Scores", min = 0, max = 1, value = 0.5)
      )
    ),
    fluidRow(
      box(width = 6, title = "SAT Data", status = "primary", solidHeader = TRUE,
          tableOutput(ns("SAT1"))
      ),
      box(width = 6, title = "SAT vs. Salary Plot", status = "primary", 
          solidHeader = TRUE, plotlyOutput(ns("plot2"))
      )
    )
  )
}

app1Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # load the data
    load("./data/SAT_2010.rda")
    
    # Render the SAT table
    output$SAT1 <- renderTable({
      SAT_2010[c(5,20,21,30,38,39,16,23,27,34,42,49), c(1,4,8,9)]
    })
    
    # render the plot
    output$plot2 <- renderPlotly({
      SAT_2010_plot2 <- SAT_2010[c(5,20,21,30,38,39,16,23,27,34,42,49),] %>%
        mutate(SAT_grp = ifelse(sat_pct <= 27, "Low SAT Participation State", 
                                "High SAT Participation State"))
      
      integer <- 8714 - input$integer * 8714
      
      low <- which(SAT_2010_plot2$salary <= 55051)
      high <- which(SAT_2010_plot2$salary > 55051)
      SAT_2010_plot2[low, ]$salary <- SAT_2010_plot2[low, ]$salary + integer
      SAT_2010_plot2[low, ]$total <- SAT_2010_plot2[low, ]$total + 
        (integer * -0.01417)
      
      SAT_2010_plot2[high, ]$salary <- SAT_2010_plot2[high, ]$salary - integer
      SAT_2010_plot2[high, ]$total <- SAT_2010_plot2[high, ]$total - 
        (integer * -0.01417)
      
      p <- ggplot(data = SAT_2010_plot2, 
                  aes(salary, total, col = SAT_grp, label = state)) +
                  geom_smooth(method = "lm", se = FALSE) +
                  labs(x = "Teachers' Salaries", y = "SAT Scores") +
                  geom_point(aes(text = paste("State:", state, "\n",
                                    "New Salary:", salary, "\n",
                                    "New SAT Score:", total, "\n",
                                    "", SAT_grp)), size = 3, pch = 21)
      
      pp <- p + geom_smooth(method = "lm", se = FALSE, color = "black", 
                            linetype = "longdash", lwd = 1.5) +
        scale_colour_manual(name = '', values = 
                              c('Low SAT Participation State' = 'red', 
                                'High SAT Participation State' = 'blue',
                                'black' = 'black')) + theme_bw() +
        theme(axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"),
              panel.background = element_blank(),
              legend.position = "none",
              axis.line = element_line(color = 'black'))
      
      ggplotly(pp, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
  })
}
