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
    
    HTML(
      '<head>
      <link rel="stylesheet" href="https://pages.nist.gov/nist-header-footer/css/nist-combined.css">
        <script src="https://pages.nist.gov/nist-header-footer/js/nist-header-footer.js" type="text/javascript" defer="defer"></script>
      </head>'
    ),

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
    
    if (!interactive()) {
        session$onSessionEnded(function() {
            stopApp()
            q("no")
        })
    }
    
}

shinyApp(ui = ui, server = server)
