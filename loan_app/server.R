library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)

input <- list(amount = 9000, rate = 5, default.payment = 100, goal.payment = 200)

shinyServer(function(input, output) {
  
  amount <- reactive({
    validate(need(input$amount > 0, "Please enter a positive loan amount."))
    input$amount
  })
  
  comp.rate <- reactive({
    validate(need(input$rate > 0, "Please enter a positive interest rate."))
    input$rate / 1200
  })
  
  default.payment <- reactive({
    validate(
      need(input$default.payment > amount() * comp.rate(),
           "This minimum payment would result in the loan never getting paid back..."))
    input$default.payment
  })
  
  goal.payment <- reactive({
    validate(
      need(input$goal.payment > amount() * comp.rate(),
           "This goal payment would result in the loan never getting paid back..."))
    input$goal.payment
  })
  
  months <- reactive({
    get.months <- function(payment) {
      ceiling((log(payment) - log(payment - amount() * comp.rate())) / log(1 + comp.rate()))
    }
    max(get.months(default.payment()), get.months(goal.payment()))
  })
  
  data <- reactive({
    
    default.amounts <- array(dim=months())
    default.amount <- amount()
    default.amounts[1] <- default.amount
    
    goal.amounts <- array(dim=months())
    goal.amount <- amount()
    goal.amounts[1] <- goal.amount
    
    default.interests <- array(dim=months())
    default.interest <- 0.0
    default.interests[1] <- default.interest
    
    goal.interests <- array(dim=months())
    goal.interest <- 0.0
    goal.interests[1] <- goal.interest
    
    
    for (m in 1:months()) {
      
      default.amount <- default.amount * (1 + comp.rate()) - default.payment()
      default.amount <- ifelse(default.amount > 0, default.amount, 0)
      default.amounts[m+1] <- default.amount
      
      goal.amount <- goal.amount * (1 + comp.rate()) - goal.payment()
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