library(shiny)
library(styler)

gitrepo <- "https://github.com/fgoepp/tp24-statistical-illusions"

homeUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .home-container {
          text-align: center;
          padding: 40px;
          background-color: #f8f9fa;
          border-radius: 10px;
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        }
        .home-title {
          font-size: 3em;
          color: #2E4053;
          margin-bottom: 20px;
          font-weight: bold;
        }
        .home-description {
          font-size: 1.2em;
          color: #34495E;
          margin-bottom: 20px;
          line-height: 1.6;
        }
        .home-text {
          margin-bottom: 20px;
        }
        .icon-links {
          display: flex;
          justify-content: center;
          gap: 30px;
          margin-top: 20px;
        }
        .icon-links a {
          font-size: 2em;
          color: #333;
          text-decoration: none;
          transition: color 0.3s ease;
        }
        .icon-links a:hover {
          color: #007bff;
        }
      "))
    ),
    div(
      class = "home-container",
      h1(class = "home-title", "Welcome to the Shiny App for 
         Statistical Illusions!"),
      br(),
      br(),
      div(
        class = "home-description",
        p(class = "home-text", "This project was started as part
          of the 'Teamprojekt'-course of the University of Tübingen under
          the supervision of Frieder Göppert and implemented by students Hossein 
          Sherkat (Two Children Problem), David Marx-Stölting 
          (St. Petersburg Paradox), Immanuel Müller (Monty Hall Problem)
          and Niman Deskaj (Simpson's Paradox)."),
        br(),
        p(class = "home-text", "The project aims to make
              complex statistical phenomena more understandable by providing an 
              engaging hands-on experience. Each illusion will be presented
              with an introduction, two interactive examples, the historical and 
              mathematical background, as well as references for
              further reading, if interested. New illusions can easily be added
              by following our wiki on GitHub."),
        br(),
        p(class = "home-text", "We hope you find these tools useful and
              informative. Enjoy exploring!"),
        br(),
        div(
          class = "icon-links",
          tags$a(
            href = gitrepo,
            tags$i(class = "fab fa-github")
          )
        )
      )
    )
  )
}


homeServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # no server logic for home module
  })
}
