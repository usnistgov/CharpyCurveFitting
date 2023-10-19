plotFitsUI <- function(id){
  ns = NS(id)
  
  tagList(
    br(),
    h3('Fitted Curves',align='center'),
    br(),
    fluidRow(column(8,plotOutput(ns('plot_fits')),offset=2)),
    br(),
    fluidRow(
    column(width=3,uiOutput(ns('which_fits_ui')),offset=3),
    column(width=3,uiOutput(ns('show_CIs_ui')),offset=0)
    ),
    br(),
    hr(),
    h3("Model Selection Statistics",align='center'),
    fluidRow(column(width=9,DT::dataTableOutput(ns('fit_metrics_table')),offset=1)),
    br(),
    hr(),
    h3('Parameter Estimates',align='center'),
    fluidRow(column(width=9,DT::dataTableOutput(ns('coefs_table')),offset=1)),
    br(),
    hr(),
    h3('DBTT Table (\u00B0C)',align='center'),
    fluidRow(column(width=9,DT::dataTableOutput(ns('dbtt_table')),offset=1)),
    br(),
    hr(),
    h3("Additional Characteristic Temperatures (\u00B0C)",align='center'),
    fluidRow(column(width=9,DT::dataTableOutput(ns('tpout')),offset=1))
    
  )
}

plotFitsServer <- function(id,computedResults) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$which_fits_ui <- renderUI({
        ns <- session$ns
        mods = computedResults()$mstats$mod
        mods = mods[mods %in% computedResults()$other_vars$mod2]
        checkboxGroupInput(ns('fits_to_show'),'Fits to Show',choices=correct_names(mods),selected=correct_names(mods))
      })
      
      output$show_CIs_ui <- renderUI({
        if(is.null(computedResults()$mstats)){
          return(NULL)
        }
        ns <- session$ns
        selectInput(ns('show_CIs'),'Show Confidence Bounds?',
                    choices = c('Yes','No'),
                    selected = 'No')
      })
      
      output$fit_metrics_table <- DT::renderDataTable({
        
        withProgress(message="Creating Metrics Table...",value=.5, {
          res = create_fit_metrics_table(computedResults())
        })
        
        res
        
      }, options = list(searching = FALSE, paging=FALSE))
                        #columnDefs = list(list(className = 'dt-center', targets = '_all'))))
      
      output$tpout <- DT::renderDataTable({
        
        withProgress(message="Creating Temperature Table...",value=.5, {
          res = plot_tpout(computedResults())
        })  
        
        res
        
      }, options = list(searching=FALSE, paging=FALSE))
                        #columnDefs = list(list(className = 'dt-center', targets = '_all'))))
      
      output$plot_fits <- renderPlot({
        
        req(input$fits_to_show)
        
        withProgress(message="Creating Fitted Model Plot...",value=.5, {
          res = plot_fits(computedResults(),input$fits_to_show,input$show_CIs)
        })
        
        res
        
      })
      
      
      output$coefs_table <- DT::renderDataTable({
        
        if(is.null(computedResults()$coef_ints)) {
          return(NULL)
        }
        
        withProgress(message="Creating Coefficients Table...",value=.5, {
          res = create_coefs_table(computedResults())
        })
        
        res
        
        
      }, options = list(searching=FALSE, paging=FALSE))
                        #columnDefs = list(list(className = 'dt-center', targets = '_all'))))
      
      
      output$dbtt_table <- DT::renderDataTable({
      
        withProgress(message="Creating DBTT Table...",value=.5, {
          res = create_dbtt_table(computedResults())
        })
        
        res
        
      },options = list(searching=FALSE, paging=FALSE))
      
      
      fits_info = reactive({
        # to pass to r markdown document
        list(fits_to_show = input$fits_to_show, show_CIs = input$show_CIs)
      })
      
      return(fits_info)
      
    }
  )
}

plotResidsUI <- function(id) {
  ns = NS(id)
  
  tagList(
    br(),
    #plotOutput(ns('resid_plot')),
    h3("Diagnostic Plots",align='center'),
    fluidRow(
      column(8,offset=2,align='center',plotOutput(ns('nlsres_plot'),height = '550px',width = '750px'))
    ),
    
    uiOutput(ns('which_model'),align='center'),
    hr()
  )
  
}

plotResidsServer <- function(id,computedResults) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$which_model = renderUI({
        ns <- session$ns
        mods = computedResults()$mstats$mod
        mods = mods[mods %in% computedResults()$other_vars$mod2]
        myl = as.list(mods)
        names(myl) = correct_names(mods) #e.g. 'HT':htf
        selectInput(ns('which_model'),'Model',choices=myl)
      })
      
      output$nlsres_plot = renderPlot({
        req(input$which_model)
        
        plot_resids(computedResults(),input$which_model)
      })
      
    }
  )
}

