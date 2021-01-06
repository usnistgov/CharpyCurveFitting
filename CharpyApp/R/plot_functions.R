plot_fits <- function(computedResults,fits_to_show,show_CIs) {
  mstats = computedResults()$mstats
  results = computedResults()$results
  other_vars = computedResults()$other_vars
  #other_vars: mod, temp, yy, nn, n.new, nsim, uls, uus, fit, yval, t, newt,
  #            upper_shelf, lower_shelf
  
  mod = other_vars$mod[correct_names(other_vars$mod) %in% fits_to_show]
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
  
  if(show_CIs == 'Yes' && !is.null(computedResults()$boots)) {
    boot_data = computedResults()$boots
    boot_data$model = correct_names(boot_data$model)
    boot_data = boot_data[boot_data$model %in% fits_to_show,]
    p = p + geom_ribbon(data=boot_data, aes(x=x, y=f, ymin=lwr.conf, ymax=upr.conf, fill=model), 
                        alpha=0.1, linetype=0)
  }
  
  p
}