library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

input <- list(amount = 9000, rate = 5, default.payment = 10, goal.payment = 200)

shinyServer(function(input, output) {
  
  comp.rate <- reactive({
    #     n <- switch(input$period,
    #                 monthly = 12,
    #                 quarterly = 4,
    #                 semiannually = 2,
    #                 annually = 1)
    n = 12
    input$rate / (n * 100)
  })
  
  months <- reactive({
    default.months <- ceiling(log(1 / (1 - input$amount * comp.rate() / input$default.payment)) / log(1 + comp.rate()))
    goal.months <- ceiling(log(1 / (1 - input$amount * comp.rate() / input$goal.payment)) / log(1 + comp.rate()))
    max(default.months, goal.months)
  })
  
  data <- reactive({
    
    default.amounts <- array(dim=months())
    default.amount <- input$amount
    default.amounts[1] <- default.amount
    
    goal.amounts <- array(dim=months())
    goal.amount <- input$amount
    goal.amounts[1] <- goal.amount
    
    default.interests <- array(dim=months())
    default.interest <- 0.0
    default.interests[1] <- default.interest
    
    goal.interests <- array(dim=months())
    goal.interest <- 0.0
    goal.interests[1] <- goal.interest
    
    
    for (m in 1:months()) {
      
      default.amount <- default.amount * (1 + comp.rate()) - input$default.payment
      default.amount <- ifelse(default.amount > 0, default.amount, 0)
      default.amounts[m+1] <- default.amount
      
      goal.amount <- goal.amount * (1 + comp.rate()) - input$goal.payment
      goal.amount <- ifelse(goal.amount > 0, goal.amount, 0)
      goal.amounts[m+1] <- goal.amount
      
      default.interest <- default.interest + default.amount * (1 + comp.rate()) - default.amount
      default.interests[m+1] <- default.interest
      
      goal.interest <- goal.interest + goal.amount * (1 + comp.rate()) - goal.amount
      goal.interests[m+1] <- goal.interest
      
    }
    
    amount.data <- data.frame(month = 0:months(), default = default.amounts, goal = goal.amounts) %>%
      gather(type, value, default, goal)
    
    interest.data <- data.frame(month = 0:months(), default = default.interests, goal = goal.interests) %>%
      gather(type, value, default, goal)
    
    bind_rows(mutate(amount.data, measure = "Amount Remaining"),
              mutate(interest.data, measure = "Interest Paid"))
  })
  
  output$plot <- renderPlot({
    ggplot(data(), aes(x = month, y = value, colour = type)) +
      facet_wrap(~measure) +
      geom_point() +
      scale_color_brewer(palette = "Set1") +
      theme_bw(base_size=18) +
      theme(legend.position = "none",
            text = element_text(family = "Open Sans")) +
      scale_x_continuous(name = "\nMonths") +
                         #breaks = seq(0, floor(months() / 12) * 12, by = 12)) + 
      scale_y_continuous(name = "",
                         breaks = function(limits) {seq(0, floor(limits[2] / 1000) * 1000, by = 1000)})
  })
  
  default.interest <- reactive({
    round(filter(data(), type == "default",
                 measure == "Interest Paid",
                 month == months())$value,
          digits = 2)
  })
  
  output$default_summary <- renderText({
    
    sprintf("With your minimum payment, you'll pay off the loan in %s months and pay $%s in interest.",
            max(filter(data(), type == "default", measure == "Amount Remaining", value > 0)$month),
            default.interest())
  })
  
  goal.interest <- reactive({
    round(filter(data(), type == "goal",
                 measure == "Interest Paid",
                 month == months())$value,
          digits = 2)
  })
  
  output$goal_summary <- renderText({
    
    sprintf("With your goal payment, you'll pay off the loan in %s months and pay $%s in interest.",
            max(filter(data(), type == "goal", measure == "Amount Remaining", value > 0)$month),
            goal.interest())
  })
  
  output$save <- renderText({
    sprintf("Your goal will save you $%s.", round(default.interest() - goal.interest(), 2))
  })

})