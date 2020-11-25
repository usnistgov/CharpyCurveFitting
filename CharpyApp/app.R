library(shiny)
library(shinythemes)
library(tidyverse)
library(minpack.lm)
library(ggplot2)
library(pracma)

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
                tabPanel('Other Plots', 
                         plotResidsUI('resids'),
                         plotCoefsTableUI('coef_table'))
            )
        )
    )
))

server <- function(input, output, session) {

    computedResults <- inputServer('start')
    
    plotFitsServer('fits_and_metrics',computedResults)
    
    plotResidsServer('resids',computedResults)
    
    plotCoefsTableServer('coef_table',computedResults)
    
}

shinyApp(ui = ui, server = server)
