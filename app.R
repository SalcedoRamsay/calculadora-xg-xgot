library(shiny)
library(ggplot2)
library(dplyr)

# Dimensiones del campo
field_width <- 68
field_length <- 105

goal_width <- 7.32
goal_height <- 2.44

calculate_xG <- function(distance) {
  return(round(76462 / (1 + exp(0.159 * (distance + 69.56))), 2))
}

calculate_xGOT <- function(distance) {
  return(round(0.128 * distance, 2))
}

ui <- fluidPage(
  titlePanel("Calculadora de xG y xGOT"),
  plotOutput("field", click = "click_field"),
  plotOutput("goal", click = "click_goal"),
  tableOutput("log_table")
)

server <- function(input, output, session) {
  log_data <- reactiveVal(data.frame(Event = character(), X = numeric(), Y = numeric(), Distance = numeric(), xG = numeric(), xGOT = numeric(), stringsAsFactors = FALSE))
  
  output$field <- renderPlot({
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = field_length, ymin = 0, ymax = field_width), fill = "green", color = "white") +
      geom_segment(aes(x = field_length / 2, xend = field_length / 2, y = 0, yend = field_width), color = "white") +
      geom_rect(aes(xmin = field_length - 5.5, xmax = field_length, ymin = (field_width - 16.5) / 2, ymax = (field_width + 16.5) / 2), fill = NA, color = "white") +
      coord_fixed()
  })
  
  output$goal <- renderPlot({
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = goal_width, ymin = 0, ymax = goal_height), fill = "gray", color = "white") +
      coord_fixed()
  })
  
  observeEvent(input$click_field, {
    x <- input$click_field$x
    y <- input$click_field$y
    
    distance <- sqrt((x - field_length)^2 + (y - field_width / 2)^2)
    xG <- calculate_xG(distance)
    
    new_entry <- data.frame(Event = "Shot", X = x, Y = y, Distance = distance, xG = xG, xGOT = NA)
    log_data(rbind(log_data(), new_entry))
  })
  
  observeEvent(input$click_goal, {
    x <- input$click_goal$x
    y <- input$click_goal$y
    
    last_shot <- tail(log_data(), 1)
    
    if (nrow(last_shot) > 0) {
      distance <- sqrt((x - goal_width / 2)^2 + y^2)
      xGOT <- calculate_xGOT(distance)
      last_shot$xGOT <- xGOT
      log_data(rbind(head(log_data(), -1), last_shot))
    }
  })
  
  output$log_table <- renderTable({
    log_data()
  })
}

shinyApp(ui, server)
