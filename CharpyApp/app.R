library(shiny)
library(shinythemes)
library(tidyverse)
library(minpack.lm)
library(ggplot2)
library(pracma)
library(truncnorm)

source('R/inputModule.R')
source('R/plotModules.R')
source('R/utils.R')
source('R/functions_4_more_fun_v12.R')

ui <- shinyUI(fluidPage(theme=shinytheme('spacelab'),

    titlePanel("Transition Curve Fitting Tool"),

    sidebarLayout(
        sidebarPanel(
            inputUI('start')
        ),

        mainPanel(
            tabsetPanel(
                tabPanel('Regression Results', 
                         plotFitsUI('fits_and_metrics')),
                tabPanel('Diagnostic Plots', 
                         plotResidsUI('resids'))
            )
        )
    )
))

server <- function(input, output, session) {

    computedResults <- inputServer('start')
    
    plotFitsServer('fits_and_metrics',computedResults)
    
    plotResidsServer('resids',computedResults)
    
}

shinyApp(ui = ui, server = server)
