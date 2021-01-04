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
    h3('Regression Coefficients',align='center'),
    DT::dataTableOutput(ns('coefs_table')),
    br(),
    hr(),
    h3('DBTT',align='center'),
    DT::dataTableOutput(ns('dbtt_table')),
    br(),
    hr(),
    h3("Additional Characteristic Temperatures",align='center'),
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
        checkboxGroupInput(ns('fits_to_show'),'Fits to Show',choices=correct_names(mods),selected=correct_names(mods))
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
        outdf$Model = correct_names(outdf$Model)
        
        outdf
        
      }, options = list(searching = FALSE, paging=FALSE))
      
      output$tpout <- DT::renderDataTable({
        
        if(length(computedResults()$tpout) == 0) {
          return(NULL)
        }
      
        outdf = dplyr::bind_rows(computedResults()$tpout)
        outdf = cbind( rep(names(computedResults()$tpout),each=length(computedResults()$other_vars$yval)) ,outdf)
        names(outdf) = c('Model','Ref Value','Temperature Est', 'SE','Lower Cl','Upper Cl')
        outdf$Model = correct_names(outdf$Model)
        outdf
        
      }, options = list(searching=FALSE, paging=FALSE))
      
      output$plot_fits <- renderPlot({
        
        req(input$fits_to_show)
        
        mstats = computedResults()$mstats
        results = computedResults()$results
        other_vars = computedResults()$other_vars
        #other_vars: mod, temp, yy, nn, n.new, nsim, uls, uus, fit, yval, t, newt,
        #            upper_shelf, lower_shelf

        mod = other_vars$mod[correct_names(other_vars$mod) %in% input$fits_to_show]
        temp = other_vars$temp
        yy = other_vars$yy
        nn = other_vars$nn
        newt = other_vars$newt
        lower_shelf = other_vars$lower_shelf
        upper_shelf = other_vars$upper_shelf
        yvar_name = other_vars$yvar_name
        
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
              
          } else if(grepl('lf$',model_name)) {
            df_to_plot$value[inds] = myfun(coef(results[[model_name]]), 
                                              newt$temp,
                                              other_vars$lower_shelf)
              
          } else if(grepl('f$',model_name)) {
            df_to_plot$value[inds] = myfun(coef(results[[model_name]]), 
                                           newt$temp,
                                           other_vars$lower_shelf,
                                           other_vars$upper_shelf)
            
          }else {
            df_to_plot$value[inds] = myfun(coef(results[[model_name]]),
                                           newt$temp)
          }
          
          
            
          
        }
        
        
        
        df_to_plot$model = correct_names(df_to_plot$model)
        
        p = ggplot(data=df_to_plot,aes(x=temp,y=value,col=model)) + 
          geom_line() +
          geom_point(data=original_data,aes(x=temp,y=yy),inherit.aes = FALSE) +
          ylab(yvar_name) +
          xlab('Temperature (\u00B0C)')+
          ggtitle(paste(yvar_name,'vs.','Temperature')) +
          theme(plot.title = element_text(hjust = 0.5))
        
        if(input$show_CIs == 'Yes' && !is.null(computedResults()$boots)) {
          boot_data = computedResults()$boots
          boot_data$model = correct_names(boot_data$model)
          boot_data = boot_data[boot_data$model %in% input$fits_to_show,]
          p = p + geom_ribbon(data=boot_data, aes(x=x, y=f, ymin=lwr.conf, ymax=upr.conf, fill=model), 
                              alpha=0.1, linetype=0)
        }
        
        p
        
      })
      
      output$coefs_table <- DT::renderDataTable({
        
        if(is.null(computedResults()$coef_ints)) {
          return(NULL)
        }
        
        other_vars = computedResults()$other_vars
        
        num_mods = length(other_vars$mod2)
        mod_names = other_vars$mod2
        
        outlist = vector(mode='list',length=num_mods)
        
        for(ii in 1:num_mods) {
          
          params = names(other_vars$start[[ mod_names[ii] ]])
          this_df = data.frame(params = params)
          coef_ints = round(computedResults()$coef_ints[[ mod_names[ii] ]],4)
          
          this_df = cbind(mod_names[ii],this_df,coef_ints[1:nrow(this_df),])
          names(this_df) = c("Model","Coefficient","Estimate","S.E.","Lower Cl", "Upper Cl")
          
          outlist[[ii]] = this_df
          
        }
        
        outdf = bind_rows(outlist)
        
        # capitalize / un-capitalize specific coefficient names
        outdf$Coefficient = toupper(outdf$Coefficient)
        inds_to_lwr = outdf$Coefficient %in% c('K','M','P')
        outdf$Coefficient[inds_to_lwr] = tolower(outdf$Coefficient[inds_to_lwr])
        
        # fix model names, e.g. htuf -> HT
        outdf$Model = correct_names(outdf$Model)
        
        the_rows = (outdf$Model %in% c("HT","AHT","ACT")) & (outdf$Coefficient == 'T0')
        outdf[the_rows,'Coefficient'] = 'DBTT'
        
        outdf
        
        
        
      }, options = list(searching=FALSE, paging=FALSE))
      
      output$dbtt_table <- DT::renderDataTable({

        outdf = dplyr::bind_rows(computedResults()$dbtt)
        outdf$Model = correct_names(outdf$Model)
        outdf
        
      },options = list(searching=FALSE, paging=FALSE))
      
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
        myl = as.list(mods)
        names(myl) = correct_names(mods) #e.g. 'HT':htf
        selectInput(ns('which_model'),'Model',choices=myl)
      })
      
      output$nlsres_plot = renderPlot({
        req(input$which_model)
        other_vars = computedResults()$other_vars
        model_name = input$which_model
        res = computedResults()$results[[model_name]]
        if(is.null(res)) {
          return(NULL)
        }
        fun = get(model_name)
        nlsres(other_vars$yy,other_vars$temp,model_name,res,fun,other_vars$lower_shelf,other_vars$upper_shelf,other_vars$fit)
      })
      
    }
  )
}

