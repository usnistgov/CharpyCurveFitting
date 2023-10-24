library(shiny)
library(readr)
library(stringr)
library(dplyr)
library(minpack.lm)
library(ggplot2)
library(pracma)
library(truncnorm)
# library(tinytex)
# 
source("R/inputModule.R")
source("R/plotModules.R")
source("R/utils.R")
source("R/functions_4_more_fun_v12.R")
source("R/downloadAllModule.R")
source("R/utils.R")

ui <- shinyUI(fluidPage(id='fullpage',
                                  
  tags$link(rel="stylesheet",href='my_style.css'),
  tags$link(rel='stylesheet',href='nist_style.css'),
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  tags$head(HTML("<title>Transition Curve Fitting</title>")),
  
  tags$div(HTML(nist_header_html)),

    HTML("<h1>&nbsp Transition Curve Fitting Tool </h1>"),

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
    ),
  
  tags$div(HTML(nist_footer_html))
  
  )

  

)

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
