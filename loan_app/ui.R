library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  
  theme = shinytheme("spacelab"),
  
  titlePanel("Loan Repayment"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      numericInput("amount", "Loan Amount", 5000, step = 100),
      numericInput("rate", "Interest Rate", 5, step = 0.1),
      #       selectInput("period", "Interest Compounded",
      #                   choices = c("monthly", "quarterly", "semiannually", "annually"),
      #                   selected = "monthly"),
      numericInput("default.payment", "Minimum Payment", 50, step = 10),
      #      numericInput("goal.payment", "Goal Payment", 200)
      sliderInput("goal.payment", "Goal Payment", pre = "$",
                  min = 0, max = 1000, value = 100, step = 10, ticks = FALSE)
#                  animate = animationOptions(interval = 1000))
    ),
    
    mainPanel(
      width = 9,
      plotOutput("plot"),
      div(align = "center",
          h5(textOutput("default_summary")),
          h5(textOutput("goal_summary")),
          h4(textOutput("save"))
      ),
      tags$head(tags$style("#default_summary{color:#e41a1c;}")),
      tags$head(tags$style("#goal_summary{color:#377eb8;}"))
    )
  )
))