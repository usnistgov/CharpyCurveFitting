compute_boot <- function(computedResults) {

  mstats = computedResults$mstats
  results = computedResults$results
  other_vars = computedResults$other_vars
  
  # generate bootstrap uncertainties and prediction/confidence bounds
  bout = list()
  coef_ints = list()
  tpout = list()
  dbtt = list()
    
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
                 other_vars$uls,
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

  
  }

  
  mod_name = names(bout)
  nrows_each = nrow(bout[[1]])
  bout = bind_rows(bout)
  bout$model = rep(mod_name,each=nrows_each)
  
  return(list('bout'=bout,'coef_ints'=coef_ints, 'tpout'=tpout, 'dbtt'=dbtt))

}

# CIs for regression mean
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

# CIs for coefficients
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
    
    # HT, AHT, BUR, ACT, KHTs
    
    if(grepl('^ht',names[ii],T)) {
      outnames[ii] = 'HT'
      
    } else if(grepl('aht',names[ii],T)) {
      outnames[ii] = 'AHT'
      
    } else if(grepl('abur',names[ii],T)) {
      outnames[ii] = 'BUR'
      
    } else if(grepl('^koh',names[ii],T)) {
      outnames[ii] = 'ACT'
      
    } else if(grepl('akoh',names[ii],T)) {
      outnames[ii] = 'KHT'
      
    }
    
  }
  
  return(outnames)
  
}

run_charpy_analysis <- function(
    dataset,
    fit, # 1=KV, 2=LE, 3=SFA, 4=other
    nboot=2000,
    c_prov=25,
    d_prov=.0001,
    t0_prov=NA, 
    t0_prov_bur=NA,
    k_prov=1,
    m_prov=.1,
    ck_prov=25, 
    p_prov=1,
    dbtt=NA,
    lower_shelf_option='Variable',
    upper_shelf_option='Variable',
    conf_level=.95,
    upper_shelf=NA,
    lower_shelf=NA,
    which_models=c('ht','aht','abur','koh','akoh'),
    num_temps=0,
    respval1=NA,
    respval2=NA,
    respval3=NA,
    custom_param=NA, 
    uls_in=0,
    uus_in=0,
    userID=NA) {
  
  start = list()
  
  if(is.na(t0_prov)) {
    t0_prov = median(dataset$temperature)
  }
  
  if(is.na(t0_prov_bur)) {
    t0_prov_bur = median(dataset$temperature)
  }
  
  if(is.na(dbtt)) {
    dbtt = median(dataset$temperature)
  }
  
  if(is.na(lower_shelf)) {
    lower_shelf = min(dataset$Y)
  }
  
  if(is.na(upper_shelf)) {
    upper_shelf = max(dataset$Y)
  }
  
  if(is.na(uls_in)) {
    uls = 0
  } else {
    uls = uls_in
  }
  
  if(is.na(uus_in)) {
    uus = 0
  } else {
    uus = uus_in
  }
  
  # for SFA, shelves fixed at 100 and 0
  upper_shelf = ifelse(fit == 3,100,upper_shelf)
  lower_shelf = ifelse(fit == 3,0,lower_shelf)
  
  # number bootstrap
  nsim = nboot
  shelves_in = c(lower_shelf_option,upper_shelf_option)
  
  if(all(shelves_in == c('Fixed','Fixed') )) {
    shelves = 'bsf'
    
  } else if(all(shelves_in == c('Fixed','Variable') )) {
    shelves = 'lsf'
    
  } else if(all(shelves_in == c('Variable','Fixed') )) {
    shelves = 'usf'
    
  } else {
    shelves = 'snf'
  }
  
  
  if('ht' %in% which_models) {
    if(shelves == 'snf') {
      start$ht    = c(c=c_prov, t0=t0_prov, lse=lower_shelf, use=upper_shelf)
      
    } else if(shelves == 'bsf') {
      start$htf   = c(c=c_prov, t0=t0_prov)
      
    } else if(shelves == 'usf') {
      start$htuf  = c(c=c_prov, t0=t0_prov, lse=lower_shelf)
      
    } else if(shelves == 'lsf') {
      start$htlf  = c(c=c_prov, t0=t0_prov, use=upper_shelf)
    }
    
  }
  
  if('aht' %in% which_models) {
    if(shelves == 'snf') {
      start$aht   = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf, use=upper_shelf)
      
    } else if(shelves == 'bsf') {
      start$ahtf  = c(c=c_prov, t0=t0_prov, d=d_prov)
      
    } else if(shelves == 'usf') {
      start$ahtuf = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf)
      
    } else if(shelves == 'lsf') {
      start$ahtlf = c(c=c_prov, t0=t0_prov, d=d_prov, use=upper_shelf)
    }
  }
  
  if('abur' %in% which_models) {
    if(shelves == 'snf') {
      start$abur   = c(k=k_prov, t0=t0_prov_bur, m=m_prov, lse=lower_shelf, use=upper_shelf)
      
    } else if(shelves == 'bsf') {
      start$aburf  = c(k=k_prov, t0=t0_prov_bur, m=m_prov) 
      
    } else if(shelves == 'usf') {
      start$aburuf = c(k=k_prov, t0=t0_prov_bur, m=m_prov, lse=lower_shelf) 
      
    } else if(shelves == 'lsf') {
      start$aburlf = c(k=k_prov, t0=t0_prov_bur, m=m_prov, use=upper_shelf) 
    }
  }
  
  if('koh' %in% which_models) {
    if(shelves == 'snf') {
      start$koh   = c(c=c_prov, DBTT=t0_prov, lse=lower_shelf, use=upper_shelf)
      
    } else if(shelves == 'bsf') {
      start$kohf  = c(c=c_prov, DBTT=t0_prov)
      
    } else if(shelves == 'usf') {
      start$kohuf = c(c=c_prov, DBTT=t0_prov, lse=lower_shelf)
      
    } else if(shelves == 'lsf') {
      start$kohlf = c(c=c_prov, DBTT=t0_prov, use=lower_shelf)
    }
  }
  
  if('akoh' %in% which_models) {
    if(shelves == 'snf') {
      start$akoh   = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf, use=upper_shelf )
      
    } else if(shelves == 'bsf') {
      start$akohf  = c(c=ck_prov, t0=dbtt, p=p_prov)
      
    } else if(shelves == 'usf') {
      start$akohuf = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf)
      
    } else if(shelves == 'lsf') {
      start$akohlf = c(c=ck_prov, t0=dbtt, p=p_prov, use=lower_shelf)
    }
  }
  
  ## translating to Jolene's variables
  mod = names(start)
  temp = dataset$temperature
  yy = dataset$Y
  nn = length(yy)
  n.new = 100
  
  if(fit < 4) {
    yvar_name = c('KV (J)','LE (mm)','SFA (%)')[fit]
  } else {
    yvar_name = custom_param
  }
  
  if(num_temps > 0) {
    yval = as.numeric(c(respval1,respval2,respval3)[1:num_temps])
    
  } else {
    yval = NA
  }
  
  # shelf uncertainties
  if (fit == 3) {
    uls = 0
    uus = 0
  } 
  
  # create new temperature values for plotting
  # increment at every degree, unless the range of degrees is less than n.new
  if(max(temp) - min(temp) < 100) {
    t = seq(round(min(temp)), round(max(temp)), length.out=n.new)
    
  } else {
    t = seq(round(min(temp)), round(max(temp)),by=1)
  }
  
  newt = data.frame(t)
  names(newt) = c("temp")
  
  # Jolene's code:
  # cat('fitting')
  fitres = fits(mod,start,lower_shelf,upper_shelf,yy,temp,fit)
  mstats = fitres[[2]]
  results = fitres[[1]]
  names(results) = mod
  
  # keep models with valid results
  nmod = length(mod)
  keepid = ifelse(mstats$conv %in% c(1,2,3), 1, 0)
  
  # save fit message for printing
  mod.not = data.frame(mstats$mod, keepid, mstats$mess)
  mod.not = mod.not[mod.not$keepid==0, c(1,3)]
  names(mod.not) = c("Model","Convergence Message")
  #mod.not
  
  # save model stats for models that converged
  mstats2 = mstats[keepid==1, 1:5]
  mod2 = as.character(mstats2$mod)
  
  
  other_vars = list(mod = mod,
                    temp = temp,
                    yy = yy,
                    nn = nn,
                    n.new = n.new,
                    nsim = nsim,
                    uus = uus,
                    uls = uls,
                    fit = fit,
                    yval = yval,
                    t = t,
                    newt = newt,
                    upper_shelf = upper_shelf,
                    lower_shelf = lower_shelf,
                    nsim = nsim,
                    mstats2 = mstats2,
                    mod2 = mod2,
                    conf_level = conf_level,
                    alpha = 1 - conf_level,
                    start = start,
                    shelves=shelves,
                    yvar_name=yvar_name,
                    userID=userID)
  
  computed_results = list(mstats=mstats,results=results,other_vars=other_vars)
  
  computed_results$flag = NA
  computed_results$msg = NA
  
  if(all(is.na(mstats$rmse))) {
    computed_results$flag = 1
    computed_results$msg = "None of the selected models converged; try rerunning with different settings."
  }
  
  if(length(mod2) > .5) {
    boots_res = compute_boot(computed_results)
    computed_results$boots = boots_res$bout
    computed_results$coef_ints = boots_res$coef_ints
    computed_results$tpout = boots_res$tpout
    computed_results$dbtt = boots_res$dbtt
  }
  
  return(computed_results)
  
}