plotUI <- function(id){
  ns = NS(id)
  
  tagList(
    br(),
    plotOutput(ns('plot_fits')),
    br(),
    uiOutput(ns('which_fits')),
  )
}

plotServer <- function(id,computedResults) {
  moduleServer(
    id,
    function(input, output, session) {
      
      output$which_fits <- renderUI({
        ns <- session$ns
        mods = computedResults()$mstats$mod
        checkboxGroupInput(ns('fits_to_show'),'Fits to Show',choices=mods,selected=mods)
      })
      
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
        p
        
      })
      
    }
  )
}