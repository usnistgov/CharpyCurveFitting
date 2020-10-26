#################################################
# generate fit results & model selection criteria
# fit results are saved globally
fits = function(mod){

# initialize output variables
nmod = length(mod)
aic = rep(NA, nmod)
bic = rep(NA, nmod)
sigT = rep(NA, nmod)
cpar = rep(NA, nmod)
rmse = rep(NA, nmod)

# counter for number of models
j = 0

for(i in mod){
  print(i)

j = j + 1
if (i=="ht"){
    rht <<- nlsLM(yy ~ a + b*tanh((temp - t0)/c), start=start.ht)
    aic[j] = AIC(rht)
    bic[j] = BIC(rht)
    rmse[j] = summary(rht)$sigma
    sigT[j] = sigmaT("free",rht)
    cpar[j] = summary(rht)$coefficients[1]
}
if (i=="aht"){
    raht <<- nlsLM(yy ~ a + b*tanh((temp - t0)/(c + d*temp)), 
            start=start.aht, lower=c(c=-Inf, t0=-Inf, a=-Inf, b=-Inf, d=0))
    aic[j] = AIC(raht)
    bic[j] = BIC(raht)
    rmse[j] = summary(raht)$sigma
    sigT[j] = sigmaT("free",raht)
    cpar[j] = summary(raht)$coefficients[1]
}
if (i=="htf"){
    rhtf <<- nlsLM(yy ~ aa + bb*tanh((temp - t0)/c), start=start.htf)
    aic[j] = AIC(rhtf)
    bic[j] = BIC(rhtf)
    rmse[j] = summary(rhtf)$sigma
    sigT[j] = sigmaT("fixed",rhtf)
    cpar[j] = summary(rhtf)$coefficients[1]

# fit zz for interval estimation
    zz = (yy - aa)/bb
    rhtf2 <<- nlsLM(zz ~ tanh((temp - t0)/c), start=start.htf)
}
if (i=="ahtf"){
    rahtf <<- nlsLM(yy ~ aa + bb*tanh((temp - t0)/(c + d*temp)), start=start.ahtf,
             lower=c(c=-Inf, t0=-Inf, d=0))
    aic[j] = AIC(rahtf)
    bic[j] = BIC(rahtf)
    rmse[j] = summary(rahtf)$sigma
    sigT[j] = sigmaT("fixed",rahtf)
    cpar[j] = summary(rahtf)$coefficients[1]

# fit zz for interval estimation
    zz = (yy - aa)/bb
    rahtf2 <<- nlsLM(zz ~ tanh((temp - t0)/(c + d*temp)), start=start.ahtf,
             lower=c(c=-Inf, t0=-Inf, d=0))
}
if (i=="abur"){
    rabur <<- nlsLM(yy ~ lse + (use - lse)*(1 + exp(-k*(temp-t0)))^(-m),
            start=start.abur)
    aic[j] = AIC(rabur)
    bic[j] = BIC(rabur)
    rmse[j] = summary(rabur)$sigma
}
if (i=="aburf"){
    raburf <<- nlsLM(yy ~ lower_shelf + (upper_shelf - lower_shelf) * 
               (1 + exp(-k*(temp-t0)))^(-m), start=start.aburf)
    aic[j] = AIC(raburf)
    bic[j] = BIC(raburf)
    rmse[j] = summary(raburf)$sigma

# fit zz for interval estimation
    zz = (yy - lower_shelf) / (upper_shelf - lower_shelf)
    raburf2 <<- nlsLM(zz ~ (1 + exp(-k*(temp-t0)))^(-m),
              start=start.aburf)
}
if (i=="koh"){
    rkoh <<- nlsLM(yy ~ (lse+use)/2 + ((use-lse)/3.141593)*atan(3.141593*(temp-DBTT)/(2*c)),
                 start=start.koh, lower=c(c=-Inf, DBTT=-Inf, lse=0, use=0))
    aic[j] = AIC(rkoh)
    bic[j] = BIC(rkoh)
    rmse[j] = summary(rkoh)$sigma
}
if (i=="kohf"){
    rkohf <<- nlsLM(yy ~ (lower_shelf + upper_shelf)/2 
           + ((upper_shelf - lower_shelf)/3.141593)*atan(3.141593*(temp - DBTT)/(2*c)),
             start=start.kohf)
    aic[j] = AIC(rkohf)
    bic[j] = BIC(rkohf)
    rmse[j] = summary(rkohf)$sigma
# fit zz for interval estimation
    zz = (yy - (lower_shelf + upper_shelf)/2) / ((upper_shelf - lower_shelf)/3.141593)
    rkohf2 <<- nlsLM(zz ~ atan(3.141593*(temp - DBTT)/(2*c)),
             start=start.kohf)
}
if (i=="akohf"){
    rakohf <<- nlsLM(yy ~ 
  as.numeric(temp <= t0)*(lower_shelf +   ((upper_shelf - lower_shelf)/(1+p))*exp((temp-t0)*(1+p)/(2*c)))
+ as.numeric(temp  > t0)*(upper_shelf - (p*(upper_shelf - lower_shelf)/(1+p))*exp(-(1+p)*(temp-t0)/(2*p*c))),
start=start.akohf)
    aic[j] = AIC(rakohf)
    bic[j] = BIC(rakohf)
    rmse[j] = summary(rakohf)$sigma
}
} 
  rmse = round(rmse,dig)
  aic = round(aic,dig)
  bic = round(bic,dig)
  sigT = round(sigT,dig)
  cpar = round(cpar,dig)
  return(data.frame(mod,rmse,aic,bic,sigT,cpar))
} ### end of fits


############################
# compute sigma T (eq. 7.36)
sigmaT = function(case,res){
if (case == "free"){
  cus  = coef(res)[3] + coef(res)[4]
  cmin = coef(res)[3] - coef(res)[4]
}
else if (case == "fixed"){
  cus  = upper_shelf
  cmin = lower_shelf
}
  id = which(yy>cmin & yy<cus)
  cc = (cus - cmin)/(yy[id] - cmin) - 1
  sqrt(sum((temp[id] - coef(res)[2] + (coef(res)[1]/2)*log(cc))^2)/length(cc))
} ### end of sigmaT


#####################################################
# plot data and predicted curves for specified models
plot.mods = function(mod){
  if (length(mod)==nmod){
      cols = colorz
  } else {
      modsub = subset(dfmod, ID %in% mod)
      cols = colorz[modsub$num]
  }
  plot(temp, yy, col="black", main=main.title, xlab="Temperature, C", 
       ylab=y.label)
  legend("topleft", bty="n", cex=0.8, legend=mod, col=cols, 
         lty=rep(1,length(mod)))
  for (i in mod){
     if(i=="ht")   points(tvals, predict(rht,newdata=newt), type="l", col=colorz[1])
     if(i=="htf")  points(tvals, predict(rhtf,newdata=newt), type="l", col=colorz[2])
     if(i=="aht")  points(tvals, predict(raht,newdata=newt), type="l", col=colorz[3])
     if(i=="ahtf") points(tvals, predict(rahtf,newdata=newt), type="l", col=colorz[4])
     if(i=="abur") points(tvals, predict(rabur,newdata=newt), type="l", col=colorz[5])
     if(i=="aburf")points(tvals, predict(raburf,newdata=newt), type="l", col=colorz[6])
     if(i=="koh")  points(tvals, predict(rkoh,newdata=newt), type="l", col=colorz[7])
     if(i=="kohf") points(tvals, predict(rkohf,newdata=newt), type="l", col=colorz[8])
     if(i=="akohf")points(tvals, predict(rakohf,newdata=newt), type="l", col=colorz[9])
  }
} ### end plot.mods


##############################
# generate 4-plot of residuals
plot4 = function(mod,res){
  mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")
  resid = yy - predict(res)
  n = length(resid)

  par(mfrow=c(2,2), cex=1, mgp=c(1.75, 0.75, 0), cex.main=1,
      mar=c(2, 2, 2, 2), oma = c(0, 0, 2, 0),
      mai = c(0.6, 0.6, 0.6, 0.6)) 
# panel 1
  plot(predict(res), resid, main="Residuals", col="red",
       ylab="Residual", xlab="Predicted")
  abline(h=0)
#  abline(lm(resid ~ temp), col="blue", lwd=2, lty=12)
# panel 2 - replace histogram with residual vs tempeature
  plot(temp, resid, main="Residuals", col="red",
       ylab="Residual", xlab="Temperature, C")
  abline(h=0)
#  hist(resid, xlab="Residual", main="Histogram", col="blue")
# panel 3
  plot(resid[1:n-1], resid[2:n], ylab="Residual[i+1]", xlab="Residual[i]",
       main="Lag Plot", col="red")
# panel 4
  qqnorm(resid, col="blue")
  qqline(resid, col="red", lwd=2)
  par(mfrow=c(1,1))
# title
  mtext(mtitle, outer = TRUE, cex = 1.25)
} ### end of plot4


###########################################
# function to plot residuals vs temperature
# and observed vs predicted
# and perform Shapiro-Wilkes test of normality
rplot = function(mod,res){
   
  resid = yy - predict(res)
  stest = shapiro.test(resid)

  par(mfrow=c(2,2), cex=0.8, mgp=c(1.75, 0.75, 0), cex.main=0.9,
      mar=c(2, 2, 2, 2), oma = c(0, 0, 2, 0),
      mai = c(0.6, 0.6, 0.6, 0.6)) 
  mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")

# panel #1
  plot(temp, resid, main=mtitle, col="red",
       ylab="Residual", xlab="Temperature, C")
  abline(h=0)
#  abline(lm(resid ~ temp), col="blue", lwd=3, lty=2)
  
# panel #2
  plot(predict(res), yy, col="blue", main="Observed vs Predicted", 
       ylab="Observed Response", xlab="Predicted Response")
  abline(a=0,b=1, col="red")

# panel #3
  plot(5,5, type="n", bty="none", xaxt="n", yaxt="n", ylab="", xlab="")
  text(5,7, "Shapiro-Wilk")
  text(5,6.5, "Test of Normality")
  text(5,5.5, paste("W = ", round(stest$statistic,dig), sep=""))
  text(5,5, paste("p-value = ", round(stest$p.value,2), sep=""))

  par(mfrow=c(1,1))
} ### end of rplot


#################################
# residual plot from nlsResiduals
nlsres = function(mod,res){
  mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")
  stdres = nlsResiduals(res)
  plot(stdres)
  mtext(mtitle, outer = TRUE, cex = 1.25)
} ### end of nlsres



#####################################################
# generate legend text containing parameter estimates
legtxt <<- function(mod,res){
if(mod=="ht"){
ltxt = c(
   paste("A=", round(coef(res)[3],dig)),
   paste("B=", round(coef(res)[4],dig)),
   paste("C=", round(coef(res)[1],dig)),
   paste("T0=",round(coef(res)[2],dig)))
} else if (mod=="aht"){
ltxt = c(
   paste("A=", round(coef(res)[3],dig)),
   paste("B=", round(coef(res)[4],dig)),
   paste("C=", round(coef(res)[1],dig)),
   paste("D=", round(coef(res)[5],dig)),
   paste("T0=",round(coef(res)[2],dig)))
} else if (mod=="htf"){
ltxt = c(
   paste("A=", aa),
   paste("B=", bb),
   paste("C=", round(coef(res)[1],dig)),
   paste("T0=",round(coef(res)[2],dig)))
} else if (mod=="ahtf"){
ltxt = c(
   paste("A=", aa),
   paste("B=", bb),
   paste("C=", round(coef(res)[1],dig)),
   paste("T0=",round(coef(res)[2],dig)),
   paste("D=", round(coef(res)[3],dig)))
} else if (mod=="abur"){
ltxt = c(
   paste("k=",  round(coef(res)[1],dig)),
   paste("T0=", round(coef(res)[2],dig)),
   paste("m=",  round(coef(res)[3],dig)),
   paste("LSE=",round(coef(res)[4],dig)),
   paste("USE=",round(coef(res)[5],dig)))
} else if (mod=="aburf"){
ltxt = c(
   paste("k=",   round(coef(res)[1],dig)),
   paste("T0=",  round(coef(res)[2],dig)),
   paste("m=",   round(coef(res)[3],dig)),
   paste("LSE=", lower_shelf),
   paste("USE=", upper_shelf))
} else if (mod=="koh"){
ltxt = c(
   paste("C=",    round(coef(res)[1],dig)),
   paste("DBTT=", round(coef(res)[2],dig)),
   paste("LSE=",  round(coef(res)[3],dig)),
   paste("USE=",  round(coef(res)[4],dig)))
} else if (mod=="kohf"){
ltxt = c(
   paste("C=",    round(coef(res)[1],dig)),
   paste("DBTT=", round(coef(res)[2],dig)),
   paste("LSE=",  lower_shelf),
   paste("USE=",  upper_shelf))
} else if (mod=="akohf"){
ltxt = c(
   paste("C=",   round(coef(res)[1],dig)),
   paste("T0=",  round(coef(res)[2],dig)),
   paste("p=",   round(coef(res)[3],dig)),
   paste("LSE=", lower_shelf),
   paste("USE=", upper_shelf))
}
  return(ltxt)
} ### end of legtxt


#########################################
# simple plot of data and predicted curve
splot = function(mod,res){
mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")
# generate legend text and title
pred = predict(res, newdata=newt)
plot(temp, yy, col="blue", main=mtitle, xlab="Temperature, C", ylab=y.label)
points(tvals, pred, type="l", col="red")
legend("topleft", bty="n", cex=0.8, legend=legtxt(mod,res))
} ### end of splot



####################################################
# generate plot with prediction or confidence bounds
pcplot = function(mod,res,bnds){
mtitle = paste(main.title,": ", dfmod[dfmod$ID==mod,3], sep="")

if (mod == "akohf") stop("Intervals not available for Kohout Asym with Fixed Shelves")

# use zz fit for intervals with fixed shelves
if (mod == "htf") res = rhtf2
if (mod == "ahtf") res = rahtf2
if (mod == "aburf") res = raburf2
if (mod == "kohf") res = rkohf2

# generate values for plotting
tv = qt(1-alpha/2, nn-length(coef(res)))
if (bnds == "p"){
  yyhatt = predictNLS(res, newdata=newt, interval="prediction")
} else {
  yyhatt = predictNLS(res, newdata=newt, interval="confidence")
} 

# compute intervals
pcsum = yyhatt$summary
pcpred = pcsum$Prop.Mean.1
pcup = pcpred + tv*pcsum$Prop.sd.1
pclo = pcpred - tv*pcsum$Prop.sd.1

# transform predictions for models with fixed shelves
if(mod %in% c("htf","ahtf")) {
    pcpred = aa + bb*pcpred
    pcup = aa + bb*pcup
    pclo = aa + bb*pclo
} else if (mod == "kohf") {
    pcpred = (lower_shelf + upper_shelf)/2 + 
          ((upper_shelf - lower_shelf)/3.141593) * pcpred
    pcup = (lower_shelf + upper_shelf)/2 + 
          ((upper_shelf - lower_shelf)/3.141593) * pcup
    pclo = (lower_shelf + upper_shelf)/2 + 
          ((upper_shelf - lower_shelf)/3.141593) * pclo
} else if (mod == "aburf") {
    pcpred = lower_shelf + (upper_shelf - lower_shelf) * pcpred
    pcup = lower_shelf + (upper_shelf - lower_shelf) * pcup
    pclo = lower_shelf + (upper_shelf - lower_shelf) * pclo
}

# generate plot
plot(temp, yy, col="black", main=mtitle,
     xlab="Temperature, C", ylab=y.label,
     sub="95 % Prediction Bounds", ylim=c(0,1.05*max(pcup)))
points(newt$temp, pcpred, type="l", lwd=2, col="red")
points(newt$temp, pcup, type="l", lwd=1, col="blue")
points(newt$temp, pclo, type="l", lwd=1, col="blue")
legend("topleft", bty="n", cex=0.8, legend=legtxt(mod,res))
} ### end of pcplot


###################################################
# compute t and u(t) given yy (one model at a time)
tfun = function(mod,res,xx){
  nsim = 1000
  nx = length(xx)

# make sure variability of t0 isn't too large compared to t0
# models are set up so that t0 or DBTT is always the second parameter
# use this until we have a better way to check
tap = abs(summary(res)$coef[2])
den = ifelse(tap < 1, 1, tap)
cv = 100*sqrt(vcov(res)[2,2])/den
tcut = 50

# parameters for random number generation
  xsd = summary(res)$sigma
  mvec = coef(res)
  vvec = vcov(res)

# compute tt and u(tt) for each value of xx
  tt = rep(NA, nx)
  ttsd = rep(NA, nx)
  for (i in 1:length(xx)){

# generate multivariate normal parameters
  xsim = rnorm(nsim, xx[i], xsd)
  mvs = as.data.frame(mvrnorm(nsim, mu=mvec, Sigma=vvec))

  if (mod=="ht") {
    mvs$a = ifelse(mvs$a < 0, 0, mvs$a)
    tt[i] = mvec[2] + (mvec[1]/2)*log((xx[i] - (mvec[3] - mvec[4])) / ((mvec[3] + mvec[4]) - xx[i]))
    if (cv < tcut) {
    ttsim = mvs$t0 + (mvs$c/2)*log((xsim - (mvs$a - mvs$b)) / ((mvs$a + mvs$b) - xsim))
    ttsd[i] = sd(ttsim, na.rm=TRUE)
    }

  } else if (mod=="htf") {
    tt[i] = mvec[2] + (mvec[1]/2)*log((xx[i] - (aa - bb)) / ((aa + bb) - xx[i]))
    if (cv < tcut) {
    ttsim = mvs$t0 + (mvs$c/2)*log((xsim - (aa - bb)) / ((aa + bb) - xsim))
    ttsd[i] = sd(ttsim, na.rm=TRUE)
    }

  } else if (mod=="aht") {
    mvs$a = ifelse(mvs$a < 0, 0, mvs$a)
    xpart = (1/2)*log((xx[i] - (mvec[3] - mvec[4])) / ((mvec[3] + mvec[4]) - xx[i]))
    tt[i] = (mvec[2] + mvec[1]*xpart) / (1 - mvec[5]*xpart)
    if (cv < tcut) {
    xpsim = (1/2)*log((xsim - (mvs$a - mvs$b)) / ((mvs$a + mvs$b) - xsim))
    ttsim = (mvs$t0 + mvs$c*xpsim) / (1 - mvs$d*xpsim)
    ttsd[i] = sd(ttsim, na.rm=TRUE)
    }

  } else if (mod=="ahtf") {
    xpart = (1/2)*log((xx[i] - (aa - bb)) / ((aa + bb) - xx[i]))
    tt[i] = (mvec[2] + mvec[1]*xpart) / (1 - mvec[3]*xpart)
    if (cv < tcut) {
    xpsim = (1/2)*log((xsim - (aa - bb)) / ((aa + bb) - xsim))
    ttsim = (mvs$t0 + mvs$c*xpsim) / (1 - mvs$d*xpsim)
    ttsd[i] = sd(ttsim, na.rm=TRUE)
    }

  } else if (mod=="abur") {
    mvs$lse = ifelse(mvs$lse < 0, 0, mvs$lse)
    tt[i] = mvec[2] - (1/mvec[1])*log( ((xx[i] - mvec[4])/(mvec[5] - mvec[4]))^(-1/mvec[3]) - 1 )
    if (cv < tcut) {
    ttsim = mvs$t0 - (1/mvs$k)*log( ((xsim - mvs$lse)/(mvs$use - mvs$lse))^(-1/mvs$m) - 1 )
    ttsd[i] = sd(ttsim, na.rm=TRUE)  
    }

  } else if (mod=="aburf") {
    tt[i] = mvec[2] - (1/mvec[1])*log( ((xx[i] - lower_shelf)/(upper_shelf - lower_shelf))^(-1/mvec[3]) - 1 )
    if (cv < tcut) {
    ttsim = mvs$t0 - (1/mvs$k)*log( ((xsim - lower_shelf)/(upper_shelf - lower_shelf))^(-1/mvs$m) - 1 )
    ttsd[i] = sd(ttsim, na.rm=TRUE)
    }  

   } else if (mod=="koh") {
     mvs$lse = ifelse(mvs$lse < 0, 0, mvs$lse)
     tt[i] = mvec[2] + (2*mvec[1]/pi)*tan((0.5*pi/(mvec[4]-mvec[3]))*(2*xx[i] - (mvec[3]+mvec[4])))
     if (cv < tcut) {
     ttsim = mvs$DBTT + (2*mvs$c/pi)*tan((0.5*pi/(mvs$use-mvs$lse))*(2*xsim - (mvs$use+mvs$lse)))
     ttsd[i] = sd(ttsim, na.rm=TRUE)
     }

   } else if (mod=="kohf") {
     tt[i] = mvec[2] + 
        (2*mvec[1]/pi)*tan((0.5*pi/(upper_shelf-lower_shelf))*(2*xx[i] - (upper_shelf+lower_shelf)))
     if (cv < tcut) {
     ttsim = mvs$DBTT + 
        (2*mvs$c/pi)*tan((0.5*pi/(upper_shelf-lower_shelf))*(2*xsim - (upper_shelf+lower_shelf)))
     ttsd[i] = sd(ttsim, na.rm=TRUE)
     }
  } 
  } # end of for loop

  return(data.frame(CVindex=cv, Value=xx, T.hat=tt, uT.hat=ttsd))
} ### end of tfun


