#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

source('R/inputModule.R')

# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme('darkly'),

    # Application title
    titlePanel("Charpy Energy Transition Curve-Fitting Tool"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            inputUI('input')
        ),

        # Show a plot of the generated distribution
        mainPanel(
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    inputServer('input')
}

# Run the application 
shinyApp(ui = ui, server = server)
