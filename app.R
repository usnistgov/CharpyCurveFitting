library(shiny)
library(shinythemes)
library(readr)
library(stringr)
library(dplyr)
library(minpack.lm)
library(ggplot2)
library(pracma)
library(truncnorm)
library(tinytex)

source('R/inputModule.R')
source('R/plotModules.R')
source('R/utils.R')
source('R/functions_4_more_fun_v12.R')
source('R/downloadAllModule.R')


ui <- shinyUI(fluidPage(theme=shinytheme('spacelab'),
                        
    
    br(),
    h4(paste("You have accessed the url for the 'Transition Curve Fitting Tool'.",
             "Usage of shinyapps.io for this application is currently pending NIST approval.",
             "Contact david.newton@nist.gov for information on using the application in",
             "the meantime (updated 5/18/2021).")),
    br(),

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

        try(removeTab(inputId='thePanel',target='Download'))

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
