library(shiny)
library(shinythemes)
library(tidyverse)
library(minpack.lm)
library(ggplot2)

source('R/inputModule.R')
source('R/plotModules.R')
source('R/utils.R')
source('R/functions_4_more_fun_v12.R')

ui <- shinyUI(fluidPage(theme=shinytheme('spacelab'),

    titlePanel("Charpy Energy Transition Curve-Fitting Tool"),

    sidebarLayout(
        sidebarPanel(
            inputUI('start')
        ),

        mainPanel(
            tabsetPanel(
                tabPanel('Model Fits', plotFitsUI('fits_and_metrics')),
                tabPanel('Residual Plots', plotResidsUI('resids'))
            )
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    computedResults <- inputServer('start')
    
    boots <- compute_boot('placeholder',computedResults)
    
    plotFitsServer('fits_and_metrics',computedResults,boots)
    
    plotResidsServer('resids',computedResults)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
