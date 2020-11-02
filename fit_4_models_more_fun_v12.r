#library(minpack.lm)
#library(MASS)
#library(beepr)
#library(ggplot2)
#library(pracma)



############################
# specify title for analysis
main.title = "Dataset 1"

#################################################
# Variable being fitted (KV = 1, LE = 2, SFA = 3)
fit <- 3

if (fit == 1) {
  main.title = paste(main.title,"KV",sep=" ")
  y.label = "Absorbed Energy KV, J"
}
if (fit == 2) {
  main.title = paste(main.title,"LE",sep=" ")
  y.label = "Lateral Expansion LE, mm"
}
if (fit == 3) {
  main.title = paste(main.title,"SFA",sep=" ")
  y.label = "Shear Fracture Appearance SFA, %"
}


##########################
# specify models to be fit
# ht : hyperbolic tangent
# htf : hyperbolic tangent with fixed shelves
# htuf:  hyperbolic tangent with fixed upper shelf
# aht : asymmetric hyperbolic tangent
# ahtf : asymmetric hyperbolic tangent with fixed shelves
# ahtuf : asymemtric hyperbolic tangent with fixed upper shelf
# abur : Burr distribution (asymmetric)
# aburf : Burr distribution (asymmetric - with fixed shelves)
# aburuf : Burr distribution (asymmetric with fixed upper shelf)
# koh : Kohout symmetric
# kohf : Kohout symmetric with fixed shelves
# kohuf : Kohout symmetric with fixed upper shelf
# akohf : Kohout asymmetric with fixed shelves
# akohuf : Kohout asymmetric with fixed upper shelf

# all models
mod = c("ht","htf","htuf",
        "aht","ahtf","ahtuf",
        "abur","aburf","aburuf",
        "koh","kohf","kohuf",
        "akoh","akohf","akohuf")

# variable shelves
# mod = c("ht","aht","abur","koh","akoh")

# fixed shelves
# mod = c("htf","ahtf","aburf","kohf","akohf")

# fixed upper shelf, variable lower shelf
# mod = c("htuf","ahtuf","aburuf","kohuf","akohuf")

# mod = "aht"

# mod = "aburf"

####################################
# specify path for working directory
# and generate file names for output
path = paste(getwd(),"/",sep="")
#path = "C:\\Charpy\\Charpy software\\R\\Transition curve fitting, prediction and confidence bounds\\R-codes\\Versions 061220\\"
outfile = paste(path, main.title, "_results.out", sep="")
#pdf(paste(path, main.title, "_plots", ".pdf", sep=""))

###################
# compile functions
#source(paste(path, "functions_4_more_fun_v12.r", sep=""))
#source("C:\\Charpy\\Charpy software\\R\\Transition curve fitting, prediction and confidence bounds\\R-codes\\Versions 061220\\functions_4_more_fun_v3.1.r")

########################
# Input data (example 1)
temp = c(-80,-50,-35,-15,0,10,21,40,76,100,148,198)

#LE
#yy = c(0.05842,0.04572,0.05842,0.07366,0.48006,0.67564,0.84836,1.13538,1.38684,1.41478,1.5,1.45)

#SFA
yy = c(0,0,6,16,28,39,53,79,100,100,100,100)

nn = length(temp)

##############################
# prediction/confidence bounds
# prediction bounds are not available
# "c" = confidence bounds
# "p" = prediction bounds - not available
# "pc" = both - not available
# "no" = none
bnds = "c"

########################
# input confidence level
alpha = 0.05

###########################################################
# input number of temperatures for fitted curves and bounds
n.new = 20

#####################################
# input number of simulated data sets
nsim = 5000

##################################
# specify values for fixed shelves
upper_shelf = 1.437905
lower_shelf = 0
if (fit == 3) {
  upper_shelf = 100
  lower_shelf = 0
}

#########################################################
# specify uncertainty of shelves for parametric bootstrap
# (simulations are needed to obtain the uncertainties of 
# the parameters when one or more shelves are fixed)
#
# uls = u(lower_shelf) will be converted to bounds of a 
# uniform distribution where the lower_shelf is bounded by [a, b]
# and a >= 0 (usually, a = 0)
# uls = (b - a)/sqrt(12)
# b = a + uls * sqrt(12)
#
# if fit ==3,
# uus = u(upper_shelf) will be converted to bounds of a 
# uniform distribution where the upper_shelf is bounded by [c, d] 
# and d <= 100 (usually, d = 100)
# uus = (d - c)/sqrt(12)
# c = d - uus*sqrt(12)
#
# if fit != 3, uus is normal(0,uus)

uls = 0.05
uus = upper_shelf*0.05


########################################
# specify starting values for each model

# hyperbolic tangent models
c_prov = 50
d_prov = 0.0001
t0_prov = 10

# burr models
k_prov = 0.1
m_prov = 1.25  

# kohout models
ck_prov = 30
p_prov = 2
dbtt = 30

start.ht    = c(c=c_prov, t0=t0_prov, lse=lower_shelf, use=upper_shelf)
start.htf   = c(c=c_prov, t0=t0_prov)
start.htuf  = c(c=c_prov, t0=t0_prov, lse=lower_shelf)

start.aht   = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf, use=upper_shelf)
start.ahtf  = c(c=c_prov, t0=t0_prov, d=d_prov)
start.ahtuf = c(c=c_prov, t0=t0_prov, d=d_prov, lse=lower_shelf)

start.abur   = c(k=k_prov, t0=t0_prov, m=m_prov, lse=lower_shelf, use=upper_shelf)
start.aburf  = c(k=k_prov, t0=t0_prov, m=m_prov) 
start.aburuf = c(k=k_prov, t0=t0_prov, m=m_prov, lse=lower_shelf) 

start.koh   = c(c=ck_prov, DBTT=t0_prov, lse=lower_shelf, use=upper_shelf)
start.kohf  = c(c=ck_prov, DBTT=t0_prov)
start.kohuf = c(c=ck_prov, DBTT=t0_prov, lse=lower_shelf)

start.akoh   = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf, use=upper_shelf )
start.akohf  = c(c=ck_prov, t0=dbtt, p=p_prov)
start.akohuf = c(c=ck_prov, t0=dbtt, p=p_prov, lse=lower_shelf)


############################################
# compute temperature for given response (y)
if (fit==1) {
  yval = c(28,41)  # Absorbed energy values of interest to nuclear community
} else if (fit==2) {
  yval = 0.89      # Lateral expansion value of interest to nuclear community
} else if (fit==3) {
  yval = 50        # Shear Fracture Appearance
}

###########################
# specify options for plots
par(cex=1.25, lwd=1.25, cex.main=1)
dig=4
colorz=c("red","blue","darkgreen","magenta","orange",
         "purple","brown","cyan","chartreuse","gray",
         "green","lightblue","violet","darkorange","gold")

############################################
# create new temperature values for plotting
t = seq(min(temp), max(temp), length.out=n.new)
newt = data.frame(t)
names(newt) = c("temp")

#######################################
# sort data by temperature for plotting
dft = data.frame(yy,temp)
dft = dft[order(dft$temp),]
temp = dft$temp
yy = dft$yy

########################################
# save model information in a data.frame
ID = c("ht","htf","htuf","aht","ahtf","ahtuf","abur","aburf",
        "aburuf","koh","kohf","kohuf","akoh","akohf","akohuf")
title = c("Symmetric TANH",   "Symmetric TANH, Fixed Shelves",
          "Symmetric TANH, Fixed Upper Shelf",
          "Asymmetric TANH",  "Asymmetric TANH, Fixed Shelves",
          "Asymmetric TANH, Fixed Upper Shelf",
          "Burr Distribution",  "Burr Distribution, Fixed Shelves",
          "Burr Distribution, Fixed Upper Shelf",
          "Symmetric Kohout", "Symmetric Kohout, Fixed Shelves",
          "Symmetric Kohout, Fixed Upper Shelf",
          "Asymmetric Kohout", "Asymmetric Kohout, Fixed Shelves",
          "Asymmetric Kohout, Fixed Upper Shelf")
nmod = length(ID)
num = c(1:nmod)
dfmod = data.frame(num,ID,title)

##############################
# fit and plot selected models
fitres = fits(mod,lower_shelf,upper_shelf,yy,temp,fit)
mstats = fitres[[2]]
results = fitres[[1]]
names(results) = mod
results

################################
# keep models with valid results
nmod = length(mod)
keepid = ifelse(mstats$conv %in% c(1,2,3), 1, 0)

# save fit message for printing
mod.not = data.frame(mstats$mod, keepid, mstats$mess)
mod.not = mod.not[mod.not$keepid==0, c(1,3)]
names(mod.not) = c("Model","Convergence Message")
mod.not

# save model stats for models that converged
mstats2 = mstats[keepid==1, 1:5]
mod2 = as.character(mstats2$mod)

##########################
# plot all curves together 
# (models with valid results only)
plot.mods(yy,temp,mod2,results,newt$temp,dfmod,colorz,lower_shelf,upper_shelf)

#########################################################################
# generate plots, residual plots, bootstrap confidence/prediction bounds,
# and bootstrap standard errors for models with valid results
bout = list()
tpout = list()
for(j in 1:length(mod2)){
  k = mstats2$modid[j]
  res = results[[k]]
  i = mod2[j]
  i.res = paste(i,".res",sep="")
  fun = get(i)
  fun.res = get(i.res)
  print(c(i,j,k))

# generate single plot and residual plot  
  splot(yy,temp,i,res,t,fun,dfmod,lower_shelf,upper_shelf)
  nlsres(yy,temp,i,res,fun,dfmod,lower_shelf,upper_shelf,fit)

# generate bootstrap uncertainties and prediction/confidence bounds
  bsres = boot(mod2[j],yy,temp,t,fun,fun.res,res,fit,lower_shelf,
               upper_shelf,uus,uls,nsim)

# generate plot with confidence bounds if requested
# FF contains predicted values for each bootstrap sample
  if (bnds == "c"){
     FF = bsres[[4]]
     ci.plot(mod2[j],yy,temp,t,fun,res,fit,lower_shelf,upper_shelf,
          bnds,dfmod,bsres[[4]],alpha)
  }
# predict temperature given reference yy value
  tpout[[j]] = tfun(i,res,yval,lower_shelf,upper_shelf,alpha,bsres[1],
                    nsim,fit,temp,fun)

# save results for output file
  bout[[j]] = report(i,res,alpha,bsres[1])
}


#####################################
# Write analysis results to text file
dash = "================================================================="
bline = "   "

# start saving text to file
sink(outfile)

cat(main.title, "\n")
cat(bline, "\n")
cat(paste("Alpha for Approximate Confidence Intervals = ", alpha, sep="" ), "\n")
cat(bline, "\n")

# print user-specified lower_shelf, upper_shelf and uncertainties
shelf = data.frame(Shelf = c("Lower","Upper"),
                   Value = c(lower_shelf, upper_shelf),
                   Unc. = c(uls, uus))
cat("User-specified lower and upper shelves", "\n")
print(shelf, row.names=FALSE)
cat(bline, "\n")

# print model comparison statistics
cat("Model Selection Statistics", "\n")
mstats2
cat(bline, "\n")

# print error messages from models
# that did not converge
if (length(mod) != length(mod2)){
cat("Model Fitting Convergence Messages", "\n")
print(mod.not)
cat(bline, "\n")
}

# print results for each model
for(j in 1:length(mod2)){
  k = mstats2$modid[j]
  res = results[[k]]
  i = mod2[j]
  fun = get(i)
  cat(dash, "\n")
  head = as.character(dfmod[dfmod$ID==i,]$title)
  cat(head, "\n")

# create indicator for whether the hessian is invertible
  hmess = is.character(try(solve(res$hessian), silent=TRUE)[1])

# save important output quantities based on hessian indicator
  if(hmess){
    junk = res
    dof = length(junk$fvec) - length(junk$par)
    sigma = sqrt(sum(junk$deviance^2)/dof)
  } else {
    junk = summary(res)
    dof = junk$df[2]
    sigma = junk$sigma
  }

  pnames = names(get(paste("start.",i,sep="")))
  cat(bline, "\n")

# for models with variable shelves, do not print bootstrap results
# for models with at least one variable shelf, only print bootstrap results
# if hessian is not invertible, print bootstrap results

# print parameters for models with variable shelves
  if (i %in% c("ht","aht","abur","koh","akoh") & hmess==FALSE){
      cat("Approximate Standard Errors and Confidence Intervals", "\n")
      cij = round(confint(res, level=1-alpha),4)
      row.names(junk$coefficients) = pnames
      print(cbind(junk$coefficients, cij))
      cat(paste("RMSE = ", round(sigma,4), sep=""), "\n")
      cat(paste("Error Degrees of Freedom = ", dof, sep=""), "\n")
  } else {
# print parameters for models with at least one fixed shelf
# or if the hessian is not invertible for variable shelves
      cat("Approximate Bootstrap Standard Errors and Confidence Intervals", "\n")
      tbout = bout[[j]]
      if (attributes(junk)$class == "summary.nls"){row.names(tbout) = pnames}
      print(tbout, digits=dig)
      cat(paste("RMSE = ", round(sigma,4), sep=""), "\n")
      cat(paste("Error Degrees of Freedom = ", dof, sep=""), "\n")
  }

# print predicted temperature for a given reference yy value 
  cat(bline, "\n")
  cat("Predicted temperature for a given response", "\n")
  cat("Approximate uncertainties based on parametric bootstrap", "\n")
  print(tpout[[j]], row.names=FALSE)
  if (i == "aburf"){
      dbtt = coef(res)[2] - (1/coef(res)[1])*log( 0.5^(-1/coef(res)[3]) - 1)
  } else if (i == "akohf"){
      dbtt = coef(res)[2] + (2*coef(res)[1]/(1+coef(res)[3]))*log((1+coef(res)[3])/2)
  } else {
      dbtt = coef(res)[2]
  }

# print DBTT
  cat(bline, "\n")
  cat(paste("DBTT = ",round(dbtt,2),sep=""), "\n")
  cat(bline, "\n")
}

##########################
# stop saving text to file
sink()

#####################################
# close output file containing graphs
dev.off()

################
# the big finish - can't use beep with pracma library
#beep("fanfare") 

