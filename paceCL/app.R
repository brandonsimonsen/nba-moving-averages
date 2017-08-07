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

# loading rds (dataset)
dataset <- readRDS('data/df.pace.climits.rds')


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   title = "Pace Confidence Intervals",
   
   plotOutput("confPlot"),
   
   # Sidebar with a slider input for number of bins 
   fluidRow(
          selectInput("select", label = 'Team', 
                      choices = distinct(dataset, team_name)[,1], 
                      selected = 'Utah Jazz')
        
      )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$confPlot <- renderPlot({
     pace.limits.by.team <- filter(dataset,team_name == input$select)
     
     g <- ggplot(pace.limits.by.team, aes(x = Location, y = avg))
     h <- g + geom_errorbar(aes(ymax = upper, ymin = lower), linetype = 'longdash', col = pace.limits.by.team$color2) +
       geom_point(size = 2, col = pace.limits.by.team$color2)
     i <- h + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.background = element_rect(fill = pace.limits.by.team$color1,color = pace.limits.by.team$color1), panel.grid.major.y = element_line(color=pace.limits.by.team$color3), panel.grid.minor.y = element_line(color=pace.limits.by.team$color3))
     i + ggtitle('Home vs Away Pace 90% Confidence Intervals') + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 16))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

