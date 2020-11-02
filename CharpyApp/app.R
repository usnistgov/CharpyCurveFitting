library(shiny)
library(shinythemes)
library(tidyverse)
library(minpack.lm)
library(ggplot2)

source('R/inputModule.R')
source('R/plotModules.R')
source('R/utils.R')
source('R/functions_4_more_fun_v12.R')

ui <- shinyUI(fluidPage(theme=shinytheme('darkly'),

    titlePanel("Charpy Energy Transition Curve-Fitting Tool"),

    sidebarLayout(
        sidebarPanel(
            inputUI('start')
        ),

        mainPanel(
            plotUI('plot1')
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    computedResults <- inputServer('start')
    
    plotServer('plot1',computedResults)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
