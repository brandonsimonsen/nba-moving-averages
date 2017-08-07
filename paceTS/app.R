#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(Cairo)
options(shiny.usecairo=T)

# loading data

dataset <- readRDS('data/shiny.pace.split.rds')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  title = "Team Pace by Game",
  
  plotOutput("pacePlot"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
    selectInput("select", label = 'Team', 
                choices = names(dataset), 
                selected = 'Utah Jazz')
    
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$pacePlot <- renderPlot({
    g <- ggplot(data = dataset[[input$select]][['2016-2017']], aes(x = 1:82, y = Pace))
    i <- g + geom_line(aes(y = Pace), color = dataset[[input$select]][['2016-2017']]$color2, size = 1)
    j <- i + theme(panel.background = element_rect(fill = dataset[[input$select]][['2016-2017']]$color1,color = dataset[[input$select]][['2016-2017']]$color1), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_line(color=dataset[[input$select]][['2016-2017']]$color3), panel.grid.minor.y = element_line(color=dataset[[input$select]][['2016-2017']]$color3))
    k <- j + ggtitle('Team Pace by Game') + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16)) + xlab('Game')
    k
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

