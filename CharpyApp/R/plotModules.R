plotFitsUI <- function(id){
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
    DT::dataTableOutput(ns('fit_metrics_table')),
    br(),
    hr(),
    h3("Characteristic Temperatures",align='center'),
    DT::dataTableOutput(ns('tpout'))
    
  )
}

plotFitsServer <- function(id,computedResults) {
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
        names(outdf) = c('Model','RMSE','AIC','BIC','Converged?')
        
        outdf
        
      }, options = list(searching = FALSE, paging=FALSE))
      
      output$tpout <- DT::renderDataTable({
        dplyr::bind_rows(computedResults()$tpout)
      }, options = list(searching=FALSE, paging=FALSE))
      
      
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
        
        df_to_plot$model = toupper(df_to_plot$model)
        
        p = ggplot(data=df_to_plot,aes(x=temp,y=value,col=model)) + 
          geom_line() +
          geom_point(data=original_data,aes(x=temp,y=yy),inherit.aes = FALSE) +
          ylab(yvar_name) +
          xlab('Temperature (\u00B0C)')+
          ggtitle(paste(yvar_name,'vs.','Temperature')) +
          theme(plot.title = element_text(hjust = 0.5))
        
        if(input$show_CIs == 'Yes') {
          boot_data = computedResults()$boots
          boot_data$model = toupper(boot_data$model)
          boot_data = boot_data[boot_data$model %in% toupper(input$fits_to_show),]
          p = p + geom_ribbon(data=boot_data, aes(x=x, y=f, ymin=lwr.conf, ymax=upr.conf, fill=model), 
                              alpha=0.1, linetype=0)
        }
        
        p
        
      })
      
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
        selectInput(ns('which_model'),'Model',choices=mods,selected=mods[1])
      })
      
      output$nlsres_plot = renderPlot({
        req(input$which_model)
        other_vars = computedResults()$other_vars
        model_name = input$which_model
        res = computedResults()$results[[model_name]]
        fun = get(model_name)
        nlsres(other_vars$yy,other_vars$temp,model_name,res,fun,other_vars$lower_shelf,other_vars$upper_shelf,other_vars$fit)
      })
      
      output$resid_plot = renderPlot({
        # this is currently not used, but didn't want to delete
        req(input$which_model)
        
        mstats = computedResults()$mstats
        results = computedResults()$results
        other_vars = computedResults()$other_vars
        #other_vars: mod, temp, yy, nn, n.new, nsim, uls, uus, fit, yval, t, newt,
        #            upper_shelf, lower_shelf
        # could attach the list, but might be sloppy coding
        
        mod = other_vars$mod[other_vars$mod %in% input$which_model]
        temp = other_vars$temp
        yy = other_vars$yy
        nn = other_vars$nn
        newt = other_vars$newt
        lower_shelf = other_vars$lower_shelf
        upper_shelf = other_vars$upper_shelf
        
        # prepare df for plotting
        original_data = data.frame(yy=yy,temp=temp)
        num_models = length(mod)
        df_to_plot = data.frame(temp = temp,
                                yy = yy,
                                value = 0,
                                model = mod)
        
        

        model_name = input$which_model
        myfun = eval(parse(text=model_name))
        inds = df_to_plot$model == model_name
        
        if(grepl('uf$',model_name)) {
          df_to_plot$value[inds] = myfun(coef(results[[model_name]]), 
                                         df_to_plot$temp, 
                                         other_vars$upper_shelf)
          
        } else if(grepl('f$',model_name)) {
          df_to_plot$value[inds] = myfun(coef(results[[model_name]]), 
                                         df_to_plot$temp,
                                         other_vars$lower_shelf,
                                         other_vars$upper_shelf)
          
        } else {
          df_to_plot$value[inds] = myfun(coef(results[[model_name]]),
                                         df_to_plot$temp)
        }
        
         
          
        yvar_name = c('KV','LE','SFA')[other_vars$fit]
        
        p = ggplot(data=df_to_plot,aes(x=temp,y=yy-value)) + 
          geom_point() +
          geom_hline(yintercept=0) +
          ggtitle(paste(yvar_name,': Residual Plot',sep='')) +
          theme(plot.title = element_text(hjust = 0.5)) +
          ylab("Residual") +
          xlab("Temperature")
        
        p
        
      })
      
    }
  )
}

plotCoefsTableUI <- function(id) {
  ns = NS(id)
  
  tagList(
    br(),
    h3('Regression Coefficients',align='center'),
    DT::dataTableOutput(ns('coefs_table')),
    br(),
    uiOutput(ns('which_model_ui'),align='center')
  )
}

plotCoefsTableServer <- function(id,computedResults) {
  moduleServer(
    id,
    function(input,output,session) {
      
      output$which_model_ui <- renderUI({
        if(is.null(computedResults()$coef_ints)) {
          return(NULL)
        }
        ns <- session$ns
        selectInput(ns('which_model'),"Model",choice=names(computedResults()$coef_ints))
      })
      
      output$coefs_table <- DT::renderDataTable({
        req(input$which_model)
        if(is.null(computedResults()$coef_ints)) {
          return(NULL)
        }
        
        
        # get coefficient names
        # if (input$which_model %in% c("ht","aht","abur","koh","akoh")){
        #   params = c(names(computedResults()$other_vars$start[[input$which_model]]),'sigma')
        #   
        # } else if (input$which_model %in% c("htf","ahtf","aburf","kohf","akohf")){
        #   params = c(names(computedResults()$other_vars$start[[input$which_model]]),'Lower shelf','Upper shelf','sigma')
        #   
        # } else {
        #   params = c(names(computedResults()$other_vars$start[[input$which_model]]),'Upper shelf','sigma')
        # }
        
        params = names(computedResults()$other_vars$start[[input$which_model]])
        outdf = data.frame(params = params)
        coef_ints = round(computedResults()$coef_ints[[input$which_model]],3)
        outdf = cbind(outdf,coef_ints[1:nrow(outdf),])
        names(outdf) = c("Parameters","Estimate","Lower Cl", "Upper Cl")
        #cat(print(pryr::mem_used()))
        outdf
        
      }, options = list(searching=FALSE, paging=FALSE))
    }
  )
}
