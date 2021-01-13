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
source('R/downloadAllModule.R')

ui <- shinyUI(fluidPage(theme=shinytheme('spacelab'),

    titlePanel("Transition Curve Fitting Tool"),

    sidebarLayout(
        sidebarPanel(
            inputUI('start')
        ),

        mainPanel(
            tabsetPanel(id='thePanel',
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
    
    observeEvent(computedResults()$other_vars, {
        appendTab(inputId='thePanel',
                  tabPanel('Download',
                           br(),
                           h5(paste("Click the button to generate a pdf file of all tables and plots",
                                    "displayed throughout the application.")),
                           downloadAllUI('download')))
    })
    
    fits_info <- plotFitsServer('fits_and_metrics',computedResults)
    
    plotResidsServer('resids',computedResults)
    
    downloadAllServer('download',computedResults,fits_info)
    
}

shinyApp(ui = ui, server = server)
