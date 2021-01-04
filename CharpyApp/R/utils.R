compute_boot <- function(computedResults) {

  mstats = computedResults$mstats
  results = computedResults$results
  other_vars = computedResults$other_vars
  
  # generate bootstrap uncertainties and prediction/confidence bounds
  bout = list()
  coef_ints = list()
  tpout = list()
  dbtt = list()
  
  withProgress(message = "Running Bootstrap Iterations", value = 0, {
    
    for(j in 1:length(other_vars$mod2)){
      k = other_vars$mstats2$modid[j]
      res = results[[k]]
      i = other_vars$mod2[j]
      model_name = i
      i.res = paste(i,".res",sep="")
      fun = get(i)
      fun.res = get(i.res)
      
      # generate bootstrap uncertainties and prediction/confidence bounds
      bsres = boot(other_vars$mod2[j],other_vars$yy, other_vars$temp,
                   other_vars$t,
                   fun,fun.res,res,
                   other_vars$fit,
                   other_vars$lower_shelf,
                   other_vars$upper_shelf,
                   other_vars$uus,
                   other_vars$laa,
                   other_vars$lbb,
                   other_vars$uus,
                   other_vars$nsim)
      
      # runs bootstrap and computes prediction CIs
      bout[[model_name]] = compute_boot_CIs(other_vars$mod2[j],
                                            other_vars$yy,
                                            other_vars$temp,
                                            other_vars$t,
                                            fun,
                                            res,
                                            other_vars$fit,
                                            other_vars$lower_shelf,
                                            other_vars$upper_shelf,
                                            bsres$FF,
                                            1 - other_vars$conf_level)
      
      # compute coefficient CIs
      coef_ints[[model_name]] = compute_boot_coefs(bsres$BBeta,
                                                   other_vars,
                                                   results,
                                                   model_name)
      
      #browser()
      
      if(grepl('(^ht)|(^aht)|(^koh)',model_name,ignore.case = T)) {
        cints = coef_ints[[model_name]]
        t0_row = which(names(other_vars$start[[model_name]]) %in% c('t0','DBTT'))
        dbttout = round(cints[t0_row,],4)
        names(dbttout) = c("Estimate", "S.E.", "Lower CI", "Upper CI")
        rownames(dbttout) = NULL
        dbttout = cbind(data.frame("Model"=model_name),dbttout)
        dbtt[[model_name]] = dbttout
      } else {
        dbtt[[model_name]] = dbttfun(model_name,res,bsres,1 - other_vars$conf_level)
      }
      
      
      if(any(is.na(other_vars$yval))) {
        # do nothing
      } else {
        tpout[[model_name]] = tfun(model_name,
                                   res,
                                   other_vars$yval,
                                   other_vars$lower_shelf,
                                   other_vars$upper_shelf,
                                   other_vars$alpha,
                                   bsres$BBeta,
                                   other_vars$nsim,
                                   other_vars$fit,
                                   other_vars$temp,
                                   fun,
                                   other_vars$yy)
      }

      
      incProgress(1/length(other_vars$mod2), detail = paste("Model",j,"of",length(other_vars$mod2)))
  
    }
    
  })
  
  mod_name = names(bout)
  nrows_each = nrow(bout[[1]])
  bout = bind_rows(bout)
  bout$model = rep(mod_name,each=nrows_each)
  
  return(list('bout'=bout,'coef_ints'=coef_ints, 'tpout'=tpout, 'dbtt'=dbtt))

}

compute_boot_CIs <- function(mod,yy,x,x.new,fun,res,fit,lower_shelf,upper_shelf,FF,alpha){
  
  # save important variables
  beta = coef(res)
  nn = length(x)
  npar = length(beta)
  dof = nn - npar
  tval = qt(1-alpha/2, dof)
  
  # save original data in data frame for plotting
  df.old = data.frame(x,yy)
  
  # generate predicted values
  f.new = pfun(mod,res,x.new,fun,lower_shelf,upper_shelf)
  
  # compute confidence bounds
  # basic bootstrap intervals do not always contain the predicted value,
  # use studentized bootstrap intervals instead
  
  # studentized bootstrap intervals
  #sdpred = as.vector(sapply(as.data.frame(FF), sd, na.rm = TRUE))
  #ci.lo = f.new - tval*sdpred
  #ci.hi = f.new + tval*sdpred
  ci.lo = as.vector(sapply(as.data.frame(FF),function(x) quantile(x,alpha/2,na.rm=T)))
  ci.hi = as.vector(sapply(as.data.frame(FF),function(x) quantile(x,1-alpha/2,na.rm=T)))
  df.mc = data.frame(x=x.new, f=f.new, lwr.conf=ci.lo, upr.conf=ci.hi)
  
  # confidence bounds for restricted paramters:
  # https://www.stat.berkeley.edu/~stark/SticiGui/Text/confidenceIntervals.htm
  
  # lower bound must be >= 0
  df.mc[,3] = ifelse(df.mc[,3] < 0, 0, df.mc[,3])
  
  # upper bound must be <= 100 for fit=3 (SFA, %)
  if(fit==3){ 
    df.mc[,4] = ifelse(df.mc[,4] > 100, 100, df.mc[,4])
  }
  
  return(df.mc)
}

compute_boot_coefs <- function(bbeta,other_vars,results,model_name) {
  
  SEs = apply(bbeta,2,function(x) sqrt(var(x)))
  ests = apply(bbeta,2,median)
  ests[1:length(results[[model_name]]$par)] = results[[model_name]]$par
  
  nn = length(other_vars$temp)
  npar = length(SEs)
  dof = nn - npar
  alpha = 1 - other_vars$conf_level
  
  lower_perc = apply(bbeta,2,function(x) quantile(x,alpha/2))
  upper_perc = apply(bbeta,2,function(x) quantile(x,1-alpha/2))
  
  
  
  #lowers = ests - qt(1 - alpha/2,dof)*SEs
  #uppers = ests + qt(1 - alpha/2,dof)*SEs
  
  return(data.frame(estimate = ests, stderr = SEs,lower = lower_perc, upper = upper_perc))
  
}

correct_names <- function(names) {
  
  outnames = rep("",length(names))
  
  for(ii in 1:length(names)) {
    
    # HT, AHT, BUR, ACT, KHT
    
    if(grepl('^ht',names[ii])) {
      outnames[ii] = 'HT'
      
    } else if(grepl('aht',names[ii])) {
      outnames[ii] = 'AHT'
      
    } else if(grepl('abur',names[ii])) {
      outnames[ii] = 'BUR'
      
    } else if(grepl('^koh',names[ii])) {
      outnames[ii] = 'ACT'
      
    } else if(grepl('akoh',names[ii])) {
      outnames[ii] = 'KHT'
      
    }
    
  }
  
  return(outnames)
  
}
