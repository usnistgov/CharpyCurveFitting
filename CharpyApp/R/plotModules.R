plotUI <- function(id){
  ns = NS(id)
  
  tagList(
    br(),
    h3('Fitted Curves',align='center'),
    plotOutput(ns('plot_fits')),
    br(),
    fluidRow(
    column(width=4,uiOutput(ns('which_fits_ui'))),
    column(width=4,uiOutput(ns('show_CIs_ui')))
    ),
    br(),
    hr(),
    h3("Fit Metrics",align='center'),
    DT::dataTableOutput(ns('fit_metrics_table'))
  )
}

plotServer <- function(id,computedResults,boots) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$which_fits_ui <- renderUI({
        ns <- session$ns
        mods = computedResults()$mstats$mod
        checkboxGroupInput(ns('fits_to_show'),'Fits to Show',choices=mods,selected=mods)
      })
      
      output$show_CIs_ui <- renderUI({
        if(is.null(computedResults()$mstats)){
          return(NULL)
        }
        ns <- session$ns
        selectInput(ns('show_CIs'),'Show Uncertainties?',
                    choices = c('Yes','No'),
                    selected = 'No')
      })
      
      
      output$fit_metrics_table <- DT::renderDataTable({
        mstats = computedResults()$mstats
        
        outdf = mstats[,2:6]
        outdf$conv = ifelse(outdf$conv %in% c(1,2,3),'Yes','No')
        names(outdf) = c('Model','rMSE','AIC','BIC','Converged?')
        
        outdf
        
      }, options = list(searching = FALSE))
      
      
      output$plot_fits <- renderPlot({
        
        req(input$fits_to_show)
        
        mstats = computedResults()$mstats
        results = computedResults()$results
        other_vars = computedResults()$other_vars
        #other_vars: mod, temp, yy, nn, n.new, nsim, uls, uus, fit, yval, t, newt,
        #            upper_shelf, lower_shelf
        # could attach the list, but might be sloppy coding
        
        mod = other_vars$mod[other_vars$mod %in% input$fits_to_show]
        temp = other_vars$temp
        yy = other_vars$yy
        nn = other_vars$nn
        newt = other_vars$newt
        lower_shelf = other_vars$lower_shelf
        upper_shelf = other_vars$upper_shelf
        
        # prepare df for plotting
        original_data = data.frame(yy=yy,temp=temp)
        num_models = length(mod)
        df_to_plot = data.frame(temp = rep(newt$temp,num_models),
                                value = 0,
                                model = as.factor(rep(mod,each=length(newt$temp))))
        

        for(i in 1:length(mod)) {
          model_name = mod[i]
          myfun = eval(parse(text=model_name))
          inds = df_to_plot$model == model_name

          if(grepl('uf$',model_name)) {
            df_to_plot$value[inds] = myfun(coef(results[[model_name]]), 
                                              newt$temp, 
                                              other_vars$upper_shelf)
              
          } else if(grepl('f$',model_name)) {
            df_to_plot$value[inds] = myfun(coef(results[[model_name]]), 
                                              newt$temp,
                                              other_vars$lower_shelf,
                                              other_vars$upper_shelf)
              
          } else {
            df_to_plot$value[inds] = myfun(coef(results[[model_name]]),
                                           newt$temp)
          }
          
          
            
          
        }
        
        yvar_name = c('KV','LE','SFA')[other_vars$fit]
        
        p = ggplot(data=df_to_plot,aes(x=temp,y=value,col=model)) + 
          geom_line() +
          geom_point(data=original_data,aes(x=temp,y=yy),inherit.aes = FALSE) +
          ylab(yvar_name) +
          ggtitle(paste(yvar_name,'vs.','Temperature')) +
          theme(plot.title = element_text(hjust = 0.5))
        
        if(input$show_CIs == 'Yes') {
          boot_data = boots()
          boot_data = boot_data[boot_data$model %in% input$fits_to_show,]
          p = p + geom_ribbon(data=boot_data, aes(x=x, y=f, ymin=lwr.conf, ymax=upr.conf, fill=model), 
                              alpha=0.1, linetype=0)
        }
        
        p
        
      })
      
    }
  )
}