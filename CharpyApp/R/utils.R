compute_boot <- function(computedResults) {

  mstats = computedResults$mstats
  results = computedResults$results
  other_vars = computedResults$other_vars
  
  # generate bootstrap uncertainties and prediction/confidence bounds
  bout = list()
  
  for(j in 1:length(other_vars$mod2)){
    k = other_vars$mstats2$modid[j]
    res = results[[k]]
    i = other_vars$mod2[j]
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
                 other_vars$nsim)
    
    
    
    bout[[i]] = compute_boot_CIs(other_vars$mod2[j],
                                 other_vars$yy,
                                 other_vars$temp,
                                 other_vars$t,
                                 fun,
                                 res,
                                 other_vars$fit,
                                 other_vars$lower_shelf,
                                 other_vars$upper_shelf,
                                 bsres[[4]],
                                 1 - other_vars$conf_level)

  }
  
  mod_name = names(bout)
  nrows_each = nrow(bout[[1]])
  bout = bind_rows(bout)
  bout$model = rep(mod_name,each=nrows_each)
  return(bout)

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
  sdpred = as.vector(sapply(as.data.frame(FF), sd, na.rm = TRUE))
  ci.lo = f.new - tval*sdpred
  ci.hi = f.new + tval*sdpred
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

