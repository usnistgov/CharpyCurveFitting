##################################################################
# function to generate high and low limits for parameter estimates
# lower shelf > 0, upper shelf <= 100 for fit=3
limits <- function(mod,npar,fit){
# generate lower limits for models
    low = rep(-Inf, npar)  # for htf, aburf, kohf, akohf
    if(mod %in% c("htuf","ahtf","kohuf")) low = c(-Inf, -Inf, 0)
    if(mod %in% c("ht","ahtuf")) low = c(-Inf, -Inf, 0, 0) 
    if(mod %in% c("koh")) low = c(-Inf, -Inf, 0, -Inf)
    if(mod %in% c("aburuf","akohuf")) low = c(-Inf, -Inf, -Inf, 0)
    if(mod=="aht") low = c(-Inf, -Inf, 0, 0, 0)
    if(mod %in% c("abur","akoh")) low = c(-Inf, -Inf, -Inf, 0, -Inf)
# generate upper limits for models when fit=3
    hi = rep(Inf, npar) # default is no upper limit
    if(fit==3){
       if(mod %in% c("ht","koh")) hi = c(Inf, Inf, Inf, 100)
       if(mod %in% c("aht")) hi = c(Inf, Inf, Inf, Inf, 100)
       if(mod %in% c("abur","akoh")) hi = c(Inf, Inf, Inf, Inf, 100)   
   } 
  return(cbind(low,hi))
} ### end of limits


#################################
#################################
# define functions for each model

##
ht     <- function(beta,temp){
                    aa = (beta[4] + beta[3])/2
                    bb = (beta[4] - beta[3])/2
                    aa + bb*tanh((temp - beta[2])/beta[1])}
ht.res <- function(beta,temp,yy){yy - ht(beta,temp)}

####
aht     <- function(beta,temp){
                     aa = (beta[5] + beta[4])/2
                     bb = (beta[5] - beta[4])/2
                     aa + bb*tanh((temp - beta[2])/(beta[1] + beta[3]*temp))}

aht.res <- function(beta,temp,yy){yy - aht(beta,temp)}

###
htf     <- function(beta,temp,lower_shelf,upper_shelf){
                     aa = (upper_shelf + lower_shelf)/2
                     bb = (upper_shelf - lower_shelf)/2
                     aa + bb*tanh((temp - beta[2]) / beta[1])}

htf.res  <- function(beta,temp,yy,lower_shelf,upper_shelf){
                      yy - htf(beta,temp,lower_shelf=lower_shelf,upper_shelf=upper_shelf)}

####
ahtf     <- function(beta,temp,lower_shelf,upper_shelf){
                      aa = (upper_shelf + lower_shelf)/2
                      bb = (upper_shelf - lower_shelf)/2
                      aa + bb*tanh((temp - beta[2])/(beta[1] + beta[3]*temp))}

ahtf.res <- function(beta,temp,yy,lower_shelf=lower_shelf,upper_shelf){
                      yy - ahtf(beta,temp,lower_shelf=lower_shelf,upper_shelf=upper_shelf)}

####
htuf     <- function(beta,temp,upper_shelf){
                      aa = (upper_shelf + beta[3])/2
                      bb = (upper_shelf - beta[3])/2
                      aa + bb*tanh((temp - beta[2]) / beta[1])}
htuf.res <- function(beta,temp,yy,upper_shelf){
                      yy - htuf(beta,temp,upper_shelf=upper_shelf)}

#####
ahtuf     <- function(beta,temp,upper_shelf){
                       aa = (upper_shelf + beta[4])/2
                       bb = (upper_shelf - beta[4])/2
                       aa + bb*tanh((temp - beta[2])/(beta[1] + beta[3]*temp))}
ahtuf.res <- function(beta,temp,yy,upper_shelf){
                       yy - ahtuf(beta,temp,upper_shelf=upper_shelf)}

####
abur     <- function(beta,temp){beta[4] + (beta[5] - beta[4])*
                      (1 + exp(-beta[1]*(temp-beta[2])))^(-beta[3])}
abur.res <- function(beta,temp,yy){yy - abur(beta,temp)}

#####
aburf     <- function(beta,temp,lower_shelf,upper_shelf){
                       lower_shelf + (upper_shelf - lower_shelf) * 
                       (1 + exp(-beta[1]*(temp-beta[2])))^(-beta[3])}
aburf.res <- function(beta,temp,yy,lower_shelf,upper_shelf){
                       yy - aburf(beta,temp,lower_shelf=lower_shelf, upper_shelf=upper_shelf)}

######
aburuf     <- function(beta,temp,upper_shelf){
                        beta[4] + (upper_shelf - beta[4]) * 
                        (1 + exp(-beta[1]*(temp-beta[2])))^(-beta[3])}
aburuf.res <- function(beta,temp,yy,upper_shelf){
                        yy - aburuf(beta,temp, upper_shelf=upper_shelf)}

###
koh     <- function(beta,temp){(beta[3]+beta[4])/2 + ((beta[4]-beta[3])/3.141593)*
                     atan(3.141593*(temp-beta[2])/(2*beta[1]))}
koh.res <- function(beta,temp,yy){yy - koh(beta,temp)}
       
####
kohf     <- function(beta,temp,lower_shelf,upper_shelf){
                     (lower_shelf+upper_shelf)/2 + ((upper_shelf-lower_shelf)/3.141593)*
                     atan(3.141593*(temp-beta[2])/(2*beta[1]))}
kohf.res <- function(beta,temp,yy,lower_shelf,upper_shelf){
                      yy - kohf(beta,temp,lower_shelf=lower_shelf, upper_shelf=upper_shelf)}

#####
kohuf     <- function(beta,temp,upper_shelf){
                       (beta[3]+upper_shelf)/2 + ((upper_shelf-beta[3])/3.141593)*
                       atan(3.141593*(temp-beta[2])/(2*beta[1]))}
kohuf.res <- function(beta,temp,yy,upper_shelf){
                       yy - kohuf(beta,temp,upper_shelf=upper_shelf)}

####
akoh <- function(beta,temp){
as.numeric(temp <= beta[2])*(beta[4] + ((beta[5] - beta[4])/(1+beta[3]))*
                   exp((temp-beta[2])*(1+beta[3])/(2*beta[1]))) +
as.numeric(temp  > beta[2])*(beta[5] - (beta[3]*(beta[5] - beta[4])/(1+beta[3]))*
                   exp(-(1+beta[3])*(temp-beta[2])/(2*beta[3]*beta[1])))}
akoh.res <- function(beta,temp,yy){yy - akoh(beta,temp)}

#####
akohf <- function(beta,temp,lower_shelf,upper_shelf){
as.numeric(temp <= beta[2])*(lower_shelf + ((upper_shelf - lower_shelf)/(1+beta[3]))*
                   exp((temp-beta[2])*(1+beta[3])/(2*beta[1]))) +
as.numeric(temp  > beta[2])*(upper_shelf - (beta[3]*(upper_shelf - lower_shelf)/(1+beta[3]))*
                   exp(-(1+beta[3])*(temp-beta[2])/(2*beta[3]*beta[1])))}
akohf.res <- function(beta,temp,yy,lower_shelf,upper_shelf){
                       yy - akohf(beta,temp,lower_shelf=lower_shelf,upper_shelf=upper_shelf)}

######
akohuf <- function(beta,temp,upper_shelf){
as.numeric(temp <= beta[2])*(beta[4] + ((upper_shelf - beta[4])/(1+beta[3]))*
                   exp((temp-beta[2])*(1+beta[3])/(2*beta[1]))) +
as.numeric(temp  > beta[2])*(upper_shelf - (beta[3]*(upper_shelf - beta[4])/(1+beta[3]))*
                   exp(-(1+beta[3])*(temp-beta[2])/(2*beta[3]*beta[1])))}
akohuf.res <- function(beta,temp,yy,upper_shelf){
                        yy - akohuf(beta,temp,upper_shelf=upper_shelf)}


######################################
# function to compute predicted values
pfun = function(mod,res,temp,fun,lower_shelf,upper_shelf){

  beta = coef(res)
  nn = length(temp)
  npar = length(beta)

  if (mod %in% c("ht","aht","abur","koh","akoh")){  
      prval = fun(beta,temp)
    } else if (mod %in% c("htf","ahtf","aburf","kohf","akohf")){
      prval = fun(beta,temp,lower_shelf,upper_shelf)
    } else {
      prval = fun(beta,temp,upper_shelf)
    }
    return(prval)
} ### end of pfun


###################################
# function to perform model fitting
fitmod = function(mod,upper_shelf,lower_shelf,yy,temp,fun,start,fit){

  print(mod)
  nn = length(yy)
  npar = length(start)
  lims = limits(mod,npar,fit)
  low = lims[,1]
  hi = lims[,2]
  ctrl = nls.lm.control(maxiter=1000)

# fit and save results  
  if(mod %in% c("ht","aht","abur","koh","akoh")){
      rfit = nls.lm(par=start, fn=fun, temp=temp, yy=yy, lower=low, 
                    upper=hi, control=ctrl)
      resid = fun(rfit$par,temp,yy) 
  } else if(mod %in% c("htf","ahtf","aburf","kohf","akohf")){
      rfit = nls.lm(par=start, fn=fun, temp=temp, yy=yy, 
                    lower_shelf=lower_shelf, upper_shelf=upper_shelf, 
                    lower=low, upper=hi, control=ctrl)
      resid = fun(rfit$par,temp,yy,lower_shelf,upper_shelf)
  } else {
      rfit = nls.lm(par=start, fn=fun, temp=temp, yy=yy, 
                    upper_shelf=upper_shelf, lower=low, upper=hi, 
                    control=ctrl)
      resid = fun(rfit$par,temp,yy,upper_shelf)
  }

# AIC from Banks & Joyner (2017) 
# BIC from https://en.wikipedia.org/wiki/Bayesian_information_criterion
# note:  npar + 1 = number of parameters plus sigma
    conv = rfit$info
    mess = rfit$message
    if (conv %in% c(1,2,3)) {   
        rmse = sqrt( sum(rfit$fvec^2) / (nn - npar) )
        loglik = log(sum(resid^2)/nn)
#        aicc =  2*(npar+1)         + nn*loglik + 2*(npar+1)*(npar+2)/(nn-npar)
        aic =  2*(npar+1)         + nn*loglik 
        bic =    (npar+1)*log(nn) + nn*loglik 
    } else {
        rmse = NA
        aic = NA
        bic = NA
    } 
    return(list(rfit,aic,bic,rmse,mod,conv,mess)) 
} ### end of fitmod


#################################################
# generate fit results & model selection criteria
# fit results are saved globally
fits <- function(mod,start,lower_shelf,upper_shelf,yy,temp,fit){
# initialize output variables
nmod = length(mod)
modid = rep(NA, nmod)
aic = rep(NA, nmod)
bic = rep(NA, nmod)
rmse = rep(NA, nmod)
conv = rep(NA, nmod)
mess = rep(NA, nmod)
dig = 3

rout = list()
lout = list()

# counter for number of models
j = 0

for(i in mod){
j = j + 1

if (i=="ht"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,ht.res,as.numeric(start$ht),fit)}
if (i=="aht"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,aht.res,as.numeric(start$aht),fit)}
if (i=="htf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,htf.res,as.numeric(start$htf),fit)}
if (i=="htuf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,htuf.res,as.numeric(start$htuf),fit)}
if (i=="ahtf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,ahtf.res,as.numeric(start$ahtf),fit)}
if (i=="ahtuf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,ahtuf.res,as.numeric(start$ahtuf),fit)}
if (i=="abur"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,abur.res,as.numeric(start$abur),fit)}
if (i=="aburf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,aburf.res,as.numeric(start$aburf),fit)}
if (i=="aburuf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,aburuf.res,as.numeric(start$aburuf),fit)}
if (i=="koh"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,koh.res,as.numeric(start$koh),fit)}
if (i=="kohf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,kohf.res,as.numeric(start$kohf),fit)}
if (i=="kohuf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,kohuf.res,as.numeric(start$kohuf),fit)}
if (i=="akoh"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,akoh.res,as.numeric(start$akoh),fit)}
if (i=="akohf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,akohf.res,as.numeric(start$akohf),fit)} 
if (i=="akohuf"){
    rout[[j]] = fitmod(i,upper_shelf,lower_shelf,yy,temp,akohuf.res,as.numeric(start$akohuf),fit)} 

# save model statistics
    modid[j] = j
    aic[j]  = round(as.numeric(rout[[j]][2]),dig)
    bic[j]  = round(as.numeric(rout[[j]][3]),dig)
    rmse[j] = round(as.numeric(rout[[j]][4]),dig)
    conv[j] = rout[[j]][6]
    mess[j] = rout[[j]][7]
# save fit results 
    lout[j] = rout[[j]][1]

} ### end of i loop

  conv = unlist(conv)
  mess = unlist(mess)
  dfs = data.frame(modid,mod,rmse,aic,bic,conv,mess)
  dfs = dfs[order(dfs$rmse),]
  return(list(lout,dfs))
} ### end of fits


#####################################################
# plot data and predicted curves for specified models
plot.mods = function(yy,temp,mod,results,tt,dfmod,colorz,lower_shelf,upper_shelf){

  plot(temp, yy, col="black", main=main.title, xlab="Temperature, C", ylab=y.label)
  legend("topleft", bty="n", cex=0.8, legend=mod, col=colorz[1:length(mod)], lty=rep(1,length(mod)))
  j = 0
  for (i in mod){
    j = j + 1
    if(i=="ht")    points(tt, ht(coef(results$ht),tt), type="l", col=colorz[j])
    if(i=="htf")   points(tt, htf(coef(results$htf),tt,lower_shelf,upper_shelf), type="l", col=colorz[j])
    if(i=="htuf")  points(tt, htuf(coef(results$htuf),tt,upper_shelf), type="l", col=colorz[j])
    if(i=="aht")   points(tt, aht(coef(results$aht),tt), type="l", col=colorz[j])
    if(i=="ahtf")  points(tt, ahtf(coef(results$ahtf), tt,lower_shelf,upper_shelf), type="l", col=colorz[j])
    if(i=="ahtuf") points(tt, ahtuf(coef(results$ahtuf),tt,upper_shelf), type="l", col=colorz[j])
    if(i=="abur")  points(tt, abur(coef(results$abur),tt), type="l", col=colorz[j])
    if(i=="aburf") points(tt, aburf(coef(results$aburf),tt,lower_shelf,upper_shelf), type="l", col=colorz[j])
    if(i=="aburuf")points(tt, aburuf(coef(results$aburuf),tt,upper_shelf), type="l", col=colorz[j])
    if(i=="koh")   points(tt, koh(coef(results$koh),tt), type="l", col=colorz[j])
    if(i=="kohf")  points(tt, kohf(coef(results$kohf),tt,lower_shelf,upper_shelf), type="l", col=colorz[j])
    if(i=="kohuf") points(tt, kohuf(coef(results$kohuf),tt,upper_shelf), type="l", col=colorz[j])
    if(i=="akoh")  points(tt, akoh(coef(results$akoh),tt), type="l", col=colorz[j])
    if(i=="akohf") points(tt, akohf(coef(results$akohf),tt,lower_shelf,upper_shelf), type="l", col=colorz[j])
    if(i=="akohuf")points(tt, akohuf(coef(results$akohuf),tt,upper_shelf), type="l", col=colorz[j])
  }
} ### end plot.mods


#############################################
# residual plots - best four plots - use this
# use nls function to generate residuals and standardized residuals 
nlsres = function(yy,temp,mod,res,fun,dfmod,lower_shelf,upper_shelf,fit){

  beta = coef(res)
  nn = length(temp)
  npar = length(beta)

  prval = pfun(mod,res,temp,fun,lower_shelf,upper_shelf)
  resid = yy - prval

# generate standardized residuals from nls fit (Raizoshams, 2009)
# https://stackoverflow.com/questions/39167204/in-r-how-does-one-extract-the-hat-projection-influence-matrix-or-values-from-an
  H = hat(mod,res,temp,lower_shelf,upper_shelf)
  rmse = sqrt( sum(resid^2) / (length(yy) - npar) ) 
  stres = resid / (rmse*sqrt(1-diag(H)))

# Shapiro-Wilk test of normality
  stest = shapiro.test(resid)
  slab = paste("Shapiro-Wilk Test: W = ", round(stest$statistic,2), " p = ", 
               round(stest$p.value,2), sep="")

# generate residual plot
  par(mfrow=c(2,2), cex=0.8, mgp=c(1.75, 0.75, 0), cex.main=0.9,
      mar=c(4, 3, 2, 2), oma = c(0, 0, 2, 0))

# panel #1
  plot(prval, resid, main="Residual Plot", col="red",
       ylab="Residual", xlab="Predicted Value")
  abline(h=0)

# panel #2
  plot(temp, stres, main="Standardized Residuals", col="blue",
       ylab="Standardized Residual", xlab="Temperature, C")
  abline(h=c(-3,0,3))

# panel #3
  plot(resid[1:nn-1], resid[2:nn], ylab="Residual[i+1]", xlab="Residual[i]",
       main="Lag Plot", col="red")

# panel #4
  qqnorm(stres, col="blue", sub=slab)
  qqline(stres, col="red", lwd=2)

# main title
  mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")
  mtext(mtitle, outer = TRUE)

  par(mfrow=c(1,1))
} ### end of nlsres


#####################################################
# generate legend text containing parameter estimates
legtxt <- function(mod,res,lower_shelf,upper_shelf){

if(mod=="ht"){
ltxt = c(
   paste("C =", round(coef(res)[1],dig)),
   paste("T0 =",round(coef(res)[2],dig)),
   paste("LSE =", round(coef(res)[3],dig)),
   paste("USE =", round(coef(res)[4],dig)))
} else if (mod=="aht"){
ltxt = c(
   paste("C =", round(coef(res)[1],dig)),
   paste("D =", round(coef(res)[3],dig)),
   paste("T0 =",round(coef(res)[2],dig)),
   paste("LSE =", round(coef(res)[4],dig)),
   paste("USE =", round(coef(res)[5],dig)))
} else if (mod=="htf"){
ltxt = c(
   paste("Fixed LSE =", lower_shelf),
   paste("Fixed USE =", upper_shelf),
   paste("C =", round(coef(res)[1],dig)),
   paste("T0 =",round(coef(res)[2],dig)))
} else if (mod=="htuf"){
ltxt = c(
   paste("Fixed USE =", upper_shelf),
   paste("C =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("LSE =", round(coef(res)[3],dig)))
} else if (mod=="ahtf"){
ltxt = c(
   paste("Fixed LSE =", lower_shelf),
   paste("Fixed USE =", upper_shelf),
   paste("C =", round(coef(res)[1],dig)),
   paste("T0 =",round(coef(res)[2],dig)),
   paste("D =", round(coef(res)[3],dig)))
} else if (mod=="ahtuf"){
ltxt = c(
   paste("Fixed USE =", upper_shelf),
   paste("C =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("D =",   round(coef(res)[3],dig)),
   paste("LSE =", round(coef(res)[4],dig)))
} else if (mod=="abur"){
ltxt = c(
   paste("k =",  round(coef(res)[1],dig)),
   paste("T0 =", round(coef(res)[2],dig)),
   paste("m =",  round(coef(res)[3],dig)),
   paste("LSE =",round(coef(res)[4],dig)),
   paste("USE =",round(coef(res)[5],dig)))
} else if (mod=="aburf"){
ltxt = c(
   paste("Fixed LSE =", lower_shelf),
   paste("Fixed USE =", upper_shelf),
   paste("k =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("m =",   round(coef(res)[3],dig)))
} else if (mod=="aburuf"){
ltxt = c(
   paste("Fixed USE =", upper_shelf),
   paste("k =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("m =",   round(coef(res)[3],dig)),
   paste("LSE =", round(coef(res)[4],dig)))
} else if (mod=="koh"){
ltxt = c(
   paste("C =",    round(coef(res)[1],dig)),
   paste("DBTT =", round(coef(res)[2],dig)),
   paste("LSE =",  round(coef(res)[3],dig)),
   paste("USE =",  round(coef(res)[4],dig)))
} else if (mod=="kohf"){
ltxt = c(
   paste("Fixed LSE =",  lower_shelf),
   paste("Fixed USE =",  upper_shelf),
   paste("C =",    round(coef(res)[1],dig)),
   paste("DBTT =", round(coef(res)[2],dig)))
} else if (mod=="kohuf"){
ltxt = c(
   paste("Fixed USE =",  upper_shelf),
   paste("C =",    round(coef(res)[1],dig)),
   paste("DBTT =", round(coef(res)[2],dig)),
   paste("LSE =",  round(coef(res)[3],dig)))
} else if (mod=="akoh"){
ltxt = c(
   paste("C =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("p =",   round(coef(res)[3],dig)),
   paste("LSE =", round(coef(res)[4],dig)),
   paste("USE =", round(coef(res)[5],dig)))
} else if (mod=="akohf"){
ltxt = c(
   paste("Fixed LSE =", lower_shelf),
   paste("Fixed USE =", upper_shelf),
   paste("C =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("p =",   round(coef(res)[3],dig)))
} else if (mod=="akohuf"){
ltxt = c(
   paste("Fixed USE =", upper_shelf),
   paste("C =",   round(coef(res)[1],dig)),
   paste("T0 =",  round(coef(res)[2],dig)),
   paste("p =",   round(coef(res)[3],dig)),
   paste("LSE =", round(coef(res)[4],dig)))
}
  return(ltxt)
} ### end of legtxt


#########################################
# simple plot of data and predicted curve
splot = function(yy,temp,mod,res,tt,fun,dfmod,lower_shelf,upper_shelf){

mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")
pred = pfun(mod,res,tt,fun,lower_shelf,upper_shelf)

plot(temp, yy, col="blue", main=mtitle, xlab="Temperature, C", ylab=y.label)
points(tt, pred, type="l", col="red")
legend("topleft", bty="n", cex=0.8, legend=legtxt(mod,res,lower_shelf,upper_shelf))
} ### end of splot


###################################################
# compute t and u(t) given yy (one model at a time)
tfun = function(mod,res,xx,lower_shelf,upper_shelf,alpha,parms,
                nsim,fit,temp,fun){

# computed t values should be within valid range of temperature data
  lowt = min(temp)
  hit = max(temp)
  nx = length(xx)

# parameters for random number generation
  nn = length(yy)
  npar = length(coef(res))
  pred = pfun(mod,res,temp,fun,lower_shelf,upper_shelf)
  xsd = sqrt(sum((yy-pred)^2)/(nn-npar))

# save parameter estimates
  mvec = c(coef(res))

# items for confidence intervals
  dof = nn - npar
  tval = qt(1-alpha/2, dof)

# compute size of bootstrap vector
  if (mod %in% c("ht","aht","abur","koh","akoh")){
      ncols = npar + 1
      nbs = length(unlist(bsres[1])) / ncols
  } else if (mod %in% c("htf","ahtf","aburf","kohf","akohf")){
      ncols = npar + 3
      nbs = length(unlist(bsres[1])) / ncols
  } else {
     ncols = npar + 2
     nbs = length(unlist(bsres[1])) / ncols
  }

# save bootstrapped coeficients in a matrix
# all shelves should be within constraints
  mvs = matrix(unlist(parms), ncol=ncols, nrow=nbs, byrow=FALSE)
  mvs = as.data.frame(mvs)

# compute tt and u(tt) and confidence interval for each value of xx
  tt = rep(NA, nx)
  ttsd = rep(NA, nx)
  ttn = rep(NA, nx)
  ci.lo = rep(NA, nx)
  ci.hi = rep(NA, nx)
  for (i in 1:length(xx)){

# generate simulated xx values
  xsim = rnorm(nbs, xx[i], xsd)

  if (mod=="ht") {
    names(mvs) = c("c","t0","lse","use")
    L1 = (xx[i] - mvec[3]) / (mvec[4] - xx[i] )
    tt[i] = ifelse(L1 > 0, mvec[2] + (mvec[1]/2)*log(L1), NA) # predicted temp

    L2 = (xsim - mvs$lse) / (mvs$use - xsim)
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = mvs$t0 + (mvs$c/2)*L2           # temp from bootstraped parameters

  } else if (mod=="htf") {
    names(mvs) = c("c","t0","lse","use")
    L1 = (xx[i] - lower_shelf) / (upper_shelf - xx[i])
    tt[i] = ifelse(L1 > 0, mvec[2] + (mvec[1]/2)*log(L1), NA) # predicted temp

    L2 = (xsim - mvs$lse) / (mvs$use - xsim)
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = mvs$t0 + (mvs$c/2)*L2          # temp from bootstrapped parameters

  } else if (mod=="htuf") {
    names(mvs) = c("c","t0","lse","use")
    L1 = (xx[i] - mvec[3]) / (upper_shelf - xx[i])
    tt[i] = ifelse(L1 > 0, mvec[2] + (mvec[1]/2)*log(L1), NA) # predicted temp

    L2 = (xsim - mvs$lse) / (mvs$use - xsim)
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = mvs$t0 + (mvs$c/2)*L2          # temp from bootstrapped parameters

  } else if (mod=="aht") {
    names(mvs) = c("c","t0","d","lse","use")
    L1 = (xx[i] - mvec[4]) / (mvec[5] - xx[i])
    tt[i] = ifelse(L1 > 0, (mvec[2] + mvec[1]*log(L1)/2) / 
            (1 - mvec[3]*log(L1)/2), NA)               # predicted temp

    L2 = (xsim - mvs$lse) / (mvs$use - xsim)
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = (mvs$t0 + mvs$c*L2/2) / (1 - mvs$d*L2/2)   # temp from bootstrapped parameters

  } else if (mod=="ahtf") {
    names(mvs) = c("c","t0","d","lse","use")
    L1 = (xx[i] - lower_shelf) / (upper_shelf - xx[i])
    tt[i] = ifelse(L1 > 0, (mvec[2] + mvec[1]*log(L1)/2) / 
            (1 - mvec[3]*log(L1)/2), NA)                  # predicted temp

    L2 = (xsim - mvs$lse) / (mvs$use - xsim)
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = (mvs$t0 + mvs$c*L2/2) /  (1 - mvs$d*L2/2)     # temp from bootstrapped parameters

  } else if (mod=="ahtuf") {
    names(mvs) = c("c","t0","d","lse","use")
    L1 = (xx[i] - mvec[4]) / (upper_shelf - xx[i])
    tt[i] = ifelse(L1 > 0, (mvec[2] + mvec[1]*log(L1)/2) / 
            (1 - mvec[3]*log(L1)/2), NA)                  # predicted temp

    L2 = (xsim - mvs$lse) / (mvs$use - xsim)
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = (mvs$t0 + mvs$c*L2/2) / (1 - mvs$d*L2/2)      # temp from bootstrapped parameters

  } else if (mod=="abur") {
    names(mvs) = c("k","t0","m","lse","use")
    L1 = ((xx[i] - mvec[4])/(mvec[5] - mvec[4]))^(-1/mvec[3]) - 1
    tt[i] = ifelse(L1 > 0, mvec[2] - (1/mvec[1])*log(L1), NA) # predicted temp

    L2 = ((xsim - mvs$lse)/(mvs$use - mvs$lse))^(-1/mvs$m) - 1
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = mvs$t0 - (1/mvs$k)*L2          # temp from bootstrapped parameters

  } else if (mod=="aburf") {
    names(mvs) = c("k","t0","m","lse","use")
    L1 = ((xx[i] - lower_shelf)/(upper_shelf - lower_shelf))^(-1/mvec[3]) - 1
    tt[i] = ifelse(L1 > 0, mvec[2] - (1/mvec[1])*log(L1), NA)      # predicted temp

    L2 = ((xsim - mvs$lse)/(mvs$use - mvs$lse))^(-1/mvs$m) - 1
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = mvs$t0 - (1/mvs$k)*L2               # temp from bootstrapped parameters
 
  } else if (mod=="aburuf") {
    names(mvs) = c("k","t0","m","lse","use")
    L1 = ((xx[i] - mvec[4])/(upper_shelf - mvec[4]))^(-1/mvec[3]) - 1 
    tt[i] = ifelse(L1 > 0, mvec[2] - (1/mvec[1])*log(L1), NA)     # predicted temp

    L2 = ((xsim - mvs$lse) /(mvs$use - mvs$lse))^(-1/mvs$m) - 1
    L2 = log( ifelse(L2 > 0, L2, NA) )
    ttsim = mvs$t0  - (1/mvs$k)*L2             # temp from bootstrapped parameters
  
   } else if (mod=="koh") {
     names(mvs) = c("c","DBTT","lse","use")
     tt[i] = mvec[2] + (2*mvec[1]/pi)*tan((0.5*pi/(mvec[4]-mvec[3]))*
             (2*xx[i] - (mvec[3]+mvec[4])))                      # predicted temp

     ttsim = mvs$DBTT + 
             (2*mvs$c/pi)*tan((0.5*pi/(mvs$use-mvs$lse))*
             (2*xsim - (mvs$use+mvs$lse)))     # temp from bootstrapped parameters

   } else if (mod=="kohf") {
     names(mvs) = c("c","DBTT","lse","use")
     tt[i] = mvec[2] + 
            (2*mvec[1]/pi)*tan((0.5*pi/(upper_shelf-lower_shelf))*
            (2*xx[i] - (upper_shelf+lower_shelf)))                 # predicted temp

     ttsim = mvs$DBTT + 
        (2*mvs$c/pi)*tan((0.5*pi/(mvs$use-mvs$lse))*
        (2*xsim - (mvs$use+mvs$lse)))           # temp from bootstrapped parameters

   } else if (mod=="kohuf") {
     names(mvs) = c("c","DBTT","lse","use")
     tt[i] = mvec[2] + 
             (2*mvec[1]/pi)*tan((0.5*pi/(upper_shelf-mvec[3]))*
             (2*xx[i] - (upper_shelf+mvec[3])))                    # predicted temp

     ttsim = mvs$DBTT + 
             (2*mvs$c/pi)*tan((0.5*pi/(mvs$use-mvs$lse))*
             (2*xsim - (mvs$use+mvs$lse)))      # temp from bootstrapped parameters

   } else if (mod=="akoh") {
     names(mvs) = c("c","t0","p","lse","use")
     L1a = (1+mvec[3])*(xx[i]-mvec[4])/(mvec[5]-mvec[4])
     L1a = log( ifelse(L1a > 0, L1a, NA) )
     L1b = (mvec[5]-xx[i])*(1+mvec[3])/(mvec[3]*(mvec[5]-mvec[4]))
     L1b = log( ifelse(L1b > 0, L1b, NA) )
     if (xx[i] <= mvec[2]) {     
       tt[i] = mvec[2] + (2*mvec[1]/(1+mvec[3]))*L1a
     } else if (xx[i] > mvec[2]) {
       tt[i] = mvec[2] - (2*mvec[1]*mvec[3]/(1+mvec[3]))*L1b   # predicted temp
     }

     ttsim = rep(NA,nbs)
     for (j in 1:nbs){
       if(xsim[j] <= mvs$t0[j]){
          L2a = (1+mvs$p[j])*(xsim[j]-mvs$lse[j])/(mvs$use[j]-mvs$lse[j])
          L2a = log( ifelse(L2a > 0, L2a, NA) )
          tts = mvs$t0[j] + (2*mvs$c[j]/(1+mvs$p[j]))*L2a
       } else { 
          L2b = (mvs$use[j]-xsim[j])*(1+mvs$p[j])/(mvs$p[j]*(mvs$use[j]-mvs$lse[j]))
          L2b = log( ifelse(L2b > 0, L2b, NA) )
          tts = mvs$t0[j] - (2*mvs$c[j]*mvs$p[j]/(1+mvs$p[j]))*L2b
       } # end of if
          ttsim[j] = tts                     # temp from bootstrapped parameters
       } # end of for

   } else if (mod=="akohf") {
     names(mvs) = c("c","t0","p","lse","use")
     L1a = (1+mvec[3])*(xx[i]-lower_shelf)/(upper_shelf-lower_shelf)  
     L1a = log( ifelse(L1a > 0, L1a, NA) ) 
     L1b = (upper_shelf-xx[i])*(1+mvec[3])/(mvec[3]*(upper_shelf-lower_shelf))
     L1b = log( ifelse(L1b > 0, L1b, NA) )
     if (xx[i] <= mvec[2]) {  
       tt[i] = mvec[2] + (2*mvec[1]/(1+mvec[3]))*L1a
     } else if(xx[i] > mvec[2]) {
       tt[i] = mvec[2] - (2*mvec[1]*mvec[3]/(1+mvec[3]))*L1b     # predicted temp
     }

     ttsim = rep(NA, nbs)
     for(j in 1:nbs){
        if (xsim[j] <= mvs$t0[j]){
            L2a = (1+mvs$p[j])*(xsim[j]-mvs$lse[j])/(mvs$use[j]-mvs$lse[j])
            L2a = log( ifelse(L2a > 0, L2a, NA) )
            tts = mvs$t0[j] + (2*mvs$c[j]/(1+mvs$p[k]))*L2a
         } else if(xsim[j] > mvs$t0[j]){
           L2b = (mvs$use[j]-xsim[j])*(1+mvs$p[j])/(mvs$p[j]*(mvs$use[j]-mvs$lse[j]))
           L2b = log( ifelse(L2b > 0, L2b, NA) ) 
           tts = mvs$t0[j] - (2*mvs$c[j]*mvs$p[j]/(1+mvs$p[j]))*L2b
         } # end of if
         ttsim[j] = tts                       # temp from bootstrapped paramerers
       } # end of for

   } else if (mod=="akohuf") {
     names(mvs) = c("c","t0","p","lse","use")
     if (xx[i] <= mvec[2]) {
       L1a = (1+mvec[3])*(xx[i]-mvec[4])/(upper_shelf-mvec[4]) 
       L1a = log( ifelse(L1a > 0, L1a, NA) )   
       tt[i] = mvec[2] + (2*mvec[1]/(1+mvec[3]))*L1a
     } else if (xx[i] > mvec[2]) {
       L1b = (upper_shelf-xx[i])*(1+mvec[3])/(mvec[3]*(upper_shelf-mvec[4]))
       L1b = log( ifelse(L1b > 0, L1b, NA) )
       tt[i] = mvec[2] - (2*mvec[1]*mvec[3]/(1+mvec[3]))*L1b      # predicted temp
     }

       ttsim = rep(NA, nbs)
       for(j in 1:nbs){
         if (xsim[j] <= mvs$t0[j]){
            L2a = (1+mvs$p[j])*(xsim[j]-mvs$lse[j])/(mvs$use[j]-mvs$lse[j])
            L2a = log( ifelse(L2a > 0, L2a, NA) )            
            tts = mvs$t0[j] + (2*mvs$c[j]/(1+mvs$p[k]))*L2a
         } else if(xsim[j] > mvs$t0[j]){
            L2b = (mvs$use[j]-xsim[j])*(1+mvs$p[j])/(mvs$p[j]*(mvs$use[j]-mvs$lse[j])) 
            L2b = log( ifelse(L2b > 0, L2b, NA) )
            tts = mvs$t0[j] - (2*mvs$c[j]*mvs$p[j]/(1+mvs$p[j]))*L2b
         } # end of if
         ttsim[j] = tts                         # temp from bootstrapped parameters
       } # end of for

   } # end of model if

# make sure temp from bootstrapped parameters are within range of temperature data
# and are not NA before computing standard deviation
       ttsim = ttsim[!is.na(ttsim)]
       ttsim = ttsim[ttsim > lowt & ttsim < hit]
       names(ttsim) = NULL
       ttsd[i] = sd(ttsim)
       ttn[i] = length(ttsim)

# studentized bootstrap interval
       ci.lo[i] = tt[i] - tval*ttsd[i]
       ci.hi[i] = tt[i] + tval*ttsd[i]

  } # end of for loop

  return(data.frame(Ref.Value=xx, T.Ref=round(tt,2), uT.Ref=round(ttsd,2),
                    Lower.CI=round(ci.lo,2), Upper.CI=round(ci.hi,2)))
} ### end of tfun


#########################################################
# function to compute confidence intervals for parameters
# using parametric bootstrap.  
# return predicted values for plotting in ci.plot function
boot = function(mod,yy,x,x.new,fun,fun.res,res,fit,lower_shelf,upper_shelf,
                uus,uls,nsim){

# save original data in data frame
  df.old = data.frame(x,yy)
  nn = length(x)

# save important variables
  beta = coef(res)
  npar = length(beta)
  dof = nn - npar
  f.old = pfun(mod,res,x,fun,lower_shelf,upper_shelf)
  sigma = sqrt( sum( (yy - f.old)^2) / dof )

# generate upper and lower limits for model parameters
  npar = length(beta)
  lims = limits(mod,npar,fit)
  low = lims[,1]
  hi = lims[,2]
  ctrl = nls.lm.control(maxiter=1000)

# prepare data for bootstrap
  f.new = pfun(mod,res,x.new,fun,lower_shelf,upper_shelf)
  if (mod %in% c("ht","aht","abur","koh","akoh")){  
      ncols = npar + 1
  } else if (mod %in% c("htf","ahtf","aburf","kohf","akohf")){
      ncols = npar + 3
  } else {
      ncols = npar + 2
  }

df.mc <- data.frame(x=x.new, f=f.new)
L = nsim
F <- matrix(nrow=L,ncol=n.new)
BBeta <- matrix(nrow=L, ncol=ncols)

# bootstrap loop
for (l in (1:L)) {

# generate y response data
# values can't be less than zero for any response
# for SFA, ygen <= 100
  ygen <- f.old + rnorm(nn,0,sigma)
  ygen = ifelse(ygen < 0, abs(ygen), ygen)
  ygen = ifelse((fit==3 & ygen > 100), 200-ygen, ygen) 

# generate random values of upper and lower shelves
# for models with at least one fixed shelf
# given user-specified uncertainties, uls and uus
#   1.  use uniform(0, a) to generate a random lower_shelf, 
#       where a = uls*sqrt(12) (solve for a+ in Eq. 6 of GUM)
#   2.  use normal(upper_shelf, uus) to generate
#       a random upper_shelf value for all responses except for SFA
#   3.  for SFA, use uniform(b, 100) to generate a random upper_shelf,
#       where b = 100-uus*sqrt(12) (solve for a- in Eq. 6 of GUM)
  lshelf = runif(1, 0, uls*sqrt(12))
  ushelf = ifelse(fit==3, runif(1, 100-uus*sqrt(12), 100), 
                          rnorm(1, upper_shelf, uus))
  if (ushelf < lshelf) ushelf = lshelf*1.10
  
# fit model for bootstrap sample, generate statistics
# constraints on model fits should ensure that shelves
# aren't outside the acceptable range of values
  if (mod %in% c("ht","aht","abur","koh","akoh")){ 
     rl.nlm <- nls.lm(par=beta, fn=fun.res, temp=x, yy=ygen, 
                      lower=low, upper=hi, control=ctrl)
  } else if (mod %in% c("htf","ahtf","aburf","kohf","akohf")){
     rl.nlm <- nls.lm(par=beta, fn=fun.res, temp=x, yy=ygen, 
                      lower_shelf=lower_shelf, upper_shelf=upper_shelf, 
                      lower=low, upper=hi, control=ctrl)
  } else {
     rl.nlm <- nls.lm(par=beta, fn=fun.res, temp=x, yy=ygen, 
                      upper_shelf=upper_shelf, 
                      lower=low, upper=hi, control=ctrl)
  }

# for cases where there is no fitting error, save results
# otherwise go to next sample
  if (rl.nlm$info %in% c(1,2,3)){

# save values for confidence and prediction intervals
      fl = pfun(mod,rl.nlm,x.new,fun,lower_shelf,upper_shelf)
      F[l,] <- fl

# save parameter estimates, shelves, and RMSE (sig)
# (lshelf and ushelf saved for models with at least one fixed shelf)
      pred = pfun(mod,rl.nlm,x,fun,lower_shelf,upper_shelf)
      sig = sqrt(sum((yy - pred)^2) / (nn - npar))
      if (mod %in% c("ht","aht","abur","koh","akoh")){
          BBeta[l,] <- c(coef(rl.nlm), sig)
      } else if (mod %in% c("htf","ahtf","aburf","kohf","akohf")){
          BBeta[l,] <- c(coef(rl.nlm), lshelf, ushelf, sig)
      } else {
          BBeta[l,] <- c(coef(rl.nlm), ushelf, sig)
      }
  } else {
      next   ### go to next sample
  }

}   ### end of bootstrap loop

  BBeta = BBeta[!is.na(BBeta[,1]),]
  nbeta = length(BBeta[,1])
  vcvbeta = cov(BBeta[,1:(ncols-1)])
  sebeta = sqrt(diag(vcvbeta))

  return(list(BBeta,nbeta,vcvbeta,F))
} ### end of boot


##########################################
# function to plot confidence bounds using
# parametric bootstrap results
ci.plot = function(mod,yy,x,x.new,fun,res,fit,lower_shelf,upper_shelf,
                   bnds,dfmod,FF,alpha){

mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")

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
if(fit==3){ df.mc[,4] = ifelse(df.mc[,4] > 100, 100, df.mc[,4])}

# generate basic plot
  pl = ggplot(data=df.old) + geom_point(aes(x=temp,y=yy)) + ggtitle(mtitle) +
     xlab("Temperature, C") + ylab(y.label) +
     theme_bw() +  theme(plot.title = element_text(hjust = 0.5))

# add confidence bounds to plot
  Ltitle = expression(paste("Conf. \n Interval"))
  aper = paste(round(100*(1-alpha),0), "%")
  pl = pl + 
     geom_ribbon(data=df.mc, aes(x=x.new, ymin=lwr.conf, ymax=upr.conf, colour=aper), 
                 alpha=0.2, fill="#339900", linetype=0) +
     geom_line(data=df.mc, aes(x=x.new, y=f.new), colour="#339900", size=1) +
     scale_color_manual(name="Conf.Int.", values = c(aper="#339900"))

  print(pl) 
} ### end of ci.plot


#################################################
# function to print table of bootstrapped results
report = function(mod,res,alpha,bsres){

# specify the number of parameters in the model
# add one parameter for rmse
  npar = length(coef(res))
  nn = length(res$fvec)

# degrees of freedom for CI
  dof = nn - npar
  
# compute length of bootstrap vector
  if (mod %in% c("ht","aht","abur","koh","akoh")){
      ncols = npar + 1
      nbs = length(unlist(bsres[1])) / ncols
  } else if (mod %in% c("htf","ahtf","aburf","kohf","akohf")){
      ncols = npar + 3
      nbs = length(unlist(bsres[1])) / ncols
  } else {
     ncols = npar + 2
     nbs = length(unlist(bsres[1])) / ncols
  }

# save bootstrapped coeficients in a matrix
  zz = matrix(unlist(bsres[1]), ncol=ncols, nrow=nbs, byrow=FALSE)

# https://en.wikipedia.org/wiki/Bootstrapping_(statistics)
# studentized bootstap interval 
  bse = sqrt(diag(cov(zz[,1:npar])))
  tval = qt(1-alpha/2, dof)
  cval = bse*tval  
  ci = matrix(c(coef(res)-cval, coef(res)+cval), nrow=npar)

  output = as.data.frame(cbind(coef(res),bse[1:npar],ci))
  names(output) = c("Estimate", "S.E.", "Lower CI", "Upper CI")
  return(output)
} ### end of report


###########################################################
# function to compute hat matrix for standardized residuals
hat = function(mod,res,temp,lower_shelf,upper_shelf){

if(mod=="ht"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = coef(res)[3]
   use = coef(res)[4]
   gg = (temp - t0) / cc
   dc = -((use - lse)/2)*(temp - t0)*(sech(gg)^2)/(cc^2)
   dt0 = -((use - lse)/2)*(sech(gg)^2)/cc
   dlse = -(tanh((temp-t0)/cc)-1)/2
   duse = (tanh((temp-t0)/cc)+1)/2
   V = cbind(dc, dt0, dlse, duse)
}
if(mod=="htuf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = coef(res)[3]
   use = upper_shelf
   gg = (temp - t0) / cc
   dc = -((use - lse)/2)*(temp - t0)*(sech(gg)^2)/(cc^2)
   dt0 = -((use - lse)/2)*(sech(gg)^2)/cc
   dlse = -(tanh((temp-t0)/cc)-1)/2
   V = cbind(dc, dt0, dlse)
}
if(mod=="htf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = lower_shelf
   use = upper_shelf
   gg = (temp - t0) / cc
   dc = -((use - lse)/2)*(temp - t0)*(sech(gg)^2)/(cc^2)
   dt0 = -((use - lse)/2)*(sech(gg)^2)/cc
   V = cbind(dc, dt0)
}
if(mod=="aht"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   d = coef(res)[3]
   lse = coef(res)[4]
   use = coef(res)[5]
   dc = ((t0-temp)*(use-lse)*sech((temp-t0)/(cc+d*temp))^2)/(2*(cc+d*temp)^2)
   dt0 = -((use-lse)*sech((temp-t0)/(d*temp+cc))^2)/(2*(d*temp+cc))
   dd = (temp*(t0-temp)*(use-lse)*sech((temp-t0)/(temp*d+cc))^2)/(2*(temp*d+cc)^2)
   dlse = -(tanh((temp-t0)/(d*temp+cc))-1)/2
   duse = (tanh((temp-t0)/(d*temp+cc))+1)/2
   V = cbind(dc,dt0,dd,dlse,duse)
}
if(mod=="ahtuf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   d = coef(res)[3]
   lse = coef(res)[4]
   use  = upper_shelf
   dc = ((t0-temp)*(use-lse)*sech((temp-t0)/(cc+d*temp))^2)/(2*(cc+d*temp)^2)
   dt0 = -((use-lse)*sech((temp-t0)/(d*temp+cc))^2)/(2*(d*temp+cc))
   dd = (temp*(t0-temp)*(use-lse)*sech((temp-t0)/(temp*d+cc))^2)/(2*(temp*d+cc)^2)
   dlse = -(tanh((temp-t0)/(d*temp+cc))-1)/2
   V = cbind(dc,dt0,dd,dlse)
}
if(mod=="ahtf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = lower_shelf
   use = upper_shelf
   gg = (temp - t0) / cc
   dc  = -((use - lse)/2)*(temp - t0)*(sech(gg)^2)/(cc^2)
   dt0 = -((use - lse)/2)*(sech(gg)^2)/cc
   V = cbind(dc, dt0)
}
if(mod=="abur"){
   kk = coef(res)[1]
   t0 = coef(res)[2]
   mm = coef(res)[3]
   lse = coef(res)[4]
   use = coef(res)[5]
   tt0 = temp - t0
   dk = mm*(tt0)*(upper_shelf - lower_shelf)*exp(-kk*tt0)*
       (1 + exp(-kk*tt0))^(-mm-1)
   dt0 = -kk*mm*(upper_shelf - lower_shelf)*exp(-kk*tt0)*
         (1 + exp(-kk*tt0))^(-mm-1)
   dm = ((upper_shelf - lower_shelf) / ((1 + exp(-kk*tt0))^mm)) *
        log(1 + exp(-kk*tt0))
   dlse = 1-1/(exp(-kk*(temp-t0))+1)^mm
   duse = 1/(exp(-kk*(temp-t0))+1)^mm
   V = cbind(dk,dt0,dm,dlse,duse)
}
if(mod=="aburuf"){
   kk = coef(res)[1]
   t0 = coef(res)[2]
   mm = coef(res)[3]
   lse = coef(res)[4]
   use = upper_shelf
   tt0 = temp - t0
   dk = mm*(tt0)*(use - lse)*exp(-kk*tt0)*(1 + exp(-kk*tt0))^(-mm-1)
   dt0 = -kk*mm*(use - lse)*exp(-kk*tt0)*(1 + exp(-kk*tt0))^(-mm-1)
   dm = ((use - lse) / ((1 + exp(-kk*tt0))^mm))*log(1 + exp(-kk*tt0))
   dlse = 1-1/(exp(-kk*(temp-t0))+1)^mm
   V = cbind(dk,dt0,dm,dlse)
}
if(mod=="aburf"){
   kk = coef(res)[1]
   t0 = coef(res)[2]
   mm = coef(res)[3]
   lse = lower_shelf
   use = upper_shelf
   tt0 = temp - t0
   dk = mm*(tt0)*(use - lse)*exp(-kk*tt0)*(1 + exp(-kk*tt0))^(-mm-1)
   dt0 = -kk*mm*(use - lse)*exp(-kk*tt0)*(1 + exp(-kk*tt0))^(-mm-1)
   dm = ((use - lse) / ((1 + exp(-kk*tt0))^mm))*log(1 + exp(-kk*tt0))
   V = cbind(dk,dt0,dm)
}
if(mod=="koh"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = coef(res)[3]
   use = coef(res)[4]
   dc = -((temp-t0)*(use-lse))/(2*((pi^2*(temp-t0)^2)/(4*cc^2)+1)*cc^2)
   dt0 = -(use-lse)/(2*cc*((pi^2*(temp-t0)^2)/(4*cc^2)+1))
   dlse = 1/2 - atan((pi*(temp-t0))/(2*cc))/pi
   duse = atan((pi*(temp-t0))/(2*cc))/pi + 1/2
   V = cbind(dc,dt0,dlse,duse)
}
if(mod=="kohuf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = coef(res)[3]
   use = upper_shelf
   dc = -((temp-t0)*(use-lse))/(2*((pi^2*(temp-t0)^2)/(4*cc^2)+1)*cc^2)
   dt0 = -(use-lse)/(2*cc*((pi^2*(temp-t0)^2)/(4*cc^2)+1))
   dlse = 1/2 - atan((pi*(temp-t0))/(2*cc))/pi
   V = cbind(dc,dt0,dlse)
}
if(mod=="kohf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   lse = lower_shelf
   use = upper_shelf
   dc = -((temp-t0)*(use-lse))/(2*((pi^2*(temp-t0)^2)/(4*cc^2)+1)*cc^2)
   dt0 = -(use-lse)/(2*cc*((pi^2*(temp-t0)^2)/(4*cc^2)+1))
   V = cbind(dc,dt0)
}
if(mod=="akoh"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   pp = coef(res)[3]
   lse = coef(res)[4]
   use = coef(res)[5]
   diff = use - lse

   dc = rep(NA,nn)
   dt0 = rep(NA,nn)
   dp = rep(NA,nn)
   dlse = rep(NA,nn)
   duse = rep(NA,nn)

   for(i in 1:nn){
   if(temp[i] <= t0){

   ee = exp((pp+1)*(temp[i]-t0)/(2*cc))
   dc[i] = ((t0-temp[i])*diff/(2*cc^2))*ee
   dt0[i] = (-diff/(2*cc))*ee
   dp[i] = (-diff/(2*cc*(pp+1)^2))*(pp*(t0-temp[i])+t0-temp[i]+2*cc)*ee
   dlse[i] = 1-exp(((pp+1)*(temp[i]-t0))/(2*cc))/(pp+1)
   duse[i] = exp(((pp+1)*(temp[i]-t0))/(2*cc))/(pp+1)

   } else {

   ff = exp(-(pp+1)*(temp[i]-t0)/(2*pp*cc))
   dc[i] = ((t0-temp[i])*diff/(2*cc^2))*ff
   dt0[i] = (-diff/(2*cc))*ff
   dp[i] = (diff/(2*pp*cc*(1+pp)^2))*(pp*(t0-temp[i]-2*cc) + t0 - temp[i])* ff
   dlse[i] = (pp*exp(-((pp+1)*(temp[i]-t0))/(2*cc*pp)))/(pp+1)
   duse[i] = 1-(pp*exp(-((pp+1)*(temp[i]-t0))/(2*cc*pp)))/(pp+1)
   }
   }
   V = cbind(dc,dt0,dp,dlse,duse)
}
if(mod=="akohuf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   pp = coef(res)[3]
   lse = coef(res)[4]
   use = upper_shelf
   diff = use - lse

   dc = rep(NA,nn)
   dt0 = rep(NA,nn)
   dp = rep(NA,nn)
   dlse = rep(NA,nn)

   for(i in 1:nn){
   if(temp[i] <= t0){

   ee = exp((pp+1)*(temp[i]-t0)/(2*cc))
   dc[i] = ((t0-temp[i])*diff/(2*cc^2))*ee
   dt0[i] = (-diff/(2*cc))*ee
   dp[i] = (-diff/(2*cc*(pp+1)^2))*(pp*(t0-temp[i])+t0-temp[i]+2*cc)*ee
   dlse[i] = 1-exp(((pp+1)*(temp[i]-t0))/(2*cc))/(pp+1)

   } else {

   ff = exp(-(pp+1)*(temp[i]-t0)/(2*pp*cc))
   dc[i] = ((t0-temp[i])*diff/(2*cc^2))*ff
   dt0[i] = (-diff/(2*cc))*ff
   dp[i] = (diff/(2*pp*cc*(1+pp)^2))*(pp*(t0-temp[i]-2*cc) + t0 - temp[i])* ff
   dlse[i] = (pp*exp(-((pp+1)*(temp[i]-t0))/(2*cc*pp)))/(pp+1)
   }
   }
   V = cbind(dc,dt0,dp,dlse)
}
if(mod=="akohf"){
   cc = coef(res)[1]
   t0 = coef(res)[2]
   pp = coef(res)[3]
   lse = lower_shelf
   use = upper_shelf
   diff = use - lse

   dc = rep(NA,nn)
   dt0 = rep(NA,nn)
   dp = rep(NA,nn)

   for(i in 1:nn){
   if(temp[i] <= t0){

   ee = exp((pp+1)*(temp[i]-t0)/(2*cc))
   dc[i] = ((t0-temp[i])*diff/(2*cc^2))*ee
   dt0[i] = (-diff/(2*cc))*ee
   dp[i] = (-diff/(2*cc*(pp+1)^2))*(pp*(t0-temp[i])+t0-temp[i]+2*cc)*ee

   } else {

   ff = exp(-(pp+1)*(temp[i]-t0)/(2*pp*cc))
   dc[i] = ((t0-temp[i])*diff/(2*cc^2))*ff
   dt0[i] = (-diff/(2*cc))*ff
   dp[i] = (diff/(2*pp*cc*(1+pp)^2))*(pp*(t0-temp[i]-2*cc) + t0 - temp[i])* ff
   }
   }
   V = cbind(dc,dt0,dp)
}
  H = V %*% solve(t(V)%*% V) %*% t(V)
  return(H)
} ### end of hat


