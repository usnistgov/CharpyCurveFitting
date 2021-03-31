plot_fits <- function(computedResults,fits_to_show,show_CIs) {
  mstats = computedResults()$mstats
  results = computedResults()$results
  other_vars = computedResults()$other_vars
  #other_vars: mod, temp, yy, nn, n.new, nsim, uls, uus, fit, yval, t, newt,
  #            upper_shelf, lower_shelf
  
  mod = other_vars$mod[correct_names(other_vars$mod) %in% fits_to_show]
  mod = mod[mod %in% other_vars$mod2] # only keep converged models
    
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
    geom_point(data=original_data,aes(x=temp,y=yy),inherit.aes = FALSE,size=3) +
    ylab(yvar_name) +
    xlab('Temperature (\u00B0C)')+
    ggtitle(paste(strsplit(yvar_name,' ')[[1]][1],'vs.','Temperature')) +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(col="Model") +
    theme(axis.text.x = element_text(size=17),
          axis.text.y = element_text(size=17),
          axis.title.x = element_text(size=20),
          axis.title.y = element_text(size=20),
          legend.text = element_text(size=17),
          plot.title = element_text(size=20),
          legend.title = element_text(size=20))
  
  if(show_CIs == 'Yes' && !is.null(computedResults()$boots)) {
    boot_data = computedResults()$boots
    boot_data$model = correct_names(boot_data$model)
    boot_data = boot_data[boot_data$model %in% fits_to_show,]
    p = p + geom_ribbon(data=boot_data, aes(x=x, y=f, ymin=lwr.conf, ymax=upr.conf, fill=model), 
                        alpha=0.1, linetype=0) + labs(fill='Model')
  }
  
  p
}


create_coefs_table <- function(computedResults) {
  
  other_vars = computedResults()$other_vars
  
  num_mods = length(other_vars$mod2)
  mod_names = other_vars$mod2
  
  outlist = vector(mode='list',length=num_mods)
  
  for(ii in 1:num_mods) {
    
    params = names(other_vars$start[[ mod_names[ii] ]])
    this_df = data.frame(params = params)
    coef_ints = round(computedResults()$coef_ints[[ mod_names[ii] ]],4)
    
    this_df = cbind(mod_names[ii],this_df,coef_ints[1:nrow(this_df),])
    names(this_df) = c("Model","Parameter","Estimate","S.E.","Lower Cl", "Upper Cl")
    
    outlist[[ii]] = this_df
    
  }
  
  outdf = bind_rows(outlist)
  
  # capitalize / un-capitalize specific coefficient names
  outdf$Parameter = toupper(outdf$Parameter)
  inds_to_lwr = outdf$Parameter %in% c('K','M','P')
  outdf$Parameter[inds_to_lwr] = tolower(outdf$Parameter[inds_to_lwr])
  
  # fix model names, e.g. htuf -> HT
  outdf$Model = correct_names(outdf$Model)
  
  the_rows = (outdf$Model %in% c("HT","AHT","ACT")) & (outdf$Parameter == 'T0')
  outdf[the_rows,'Parameter'] = 'DBTT'
  
  outdf
  
}


create_fit_metrics_table <- function(computedResults) {
  mstats = computedResults()$mstats
  
  outdf = mstats[,2:6]
  outdf$conv = ifelse(outdf$conv %in% c(1,2,3),'Yes','No')
  names(outdf) = c('Model','RMSE','AIC','BIC','Converged?')
  outdf$Model = correct_names(outdf$Model)
  
  outdf
}


create_dbtt_table <- function(computedResults) {
  
  outdf = dplyr::bind_rows(computedResults()$dbtt)
  outdf$Model = correct_names(outdf$Model)
  outdf
  
}


plot_tpout <- function(computedResults) {
  if(length(computedResults()$tpout) == 0) {
    return(NULL)
  }
  
  outdf = dplyr::bind_rows(computedResults()$tpout)
  outdf = cbind( rep(names(computedResults()$tpout),each=length(computedResults()$other_vars$yval)) ,outdf)
  names(outdf) = c('Model','Ref Value','Temperature Est', 'SE','Lower Cl','Upper Cl')
  outdf$Model = correct_names(outdf$Model)
  outdf
}


plot_resids <- function(computedResults,model_name) {
  other_vars = computedResults()$other_vars
  res = computedResults()$results[[model_name]]
  if(is.null(res)) {
    return(NULL)
  }
  fun = get(model_name)
  nlsres(other_vars$yy,other_vars$temp,model_name,res,fun,other_vars$lower_shelf,other_vars$upper_shelf,other_vars$fit)
}

plot_resids_md <- function(other_vars,results,model_name) {
  
  res = results[[model_name]]
  
  if(is.null(res)) {
    return(NULL)
  }
  
  fun = get(model_name)
  nlsres(other_vars$yy,
         other_vars$temp,
         model_name,
         res,
         fun,
         other_vars$lower_shelf,
         other_vars$upper_shelf,
         other_vars$fit,
         for_markdown=TRUE)
}



