
rm(list = ls())
library(zoo)
library(sandwich)

##
## Attaching package: 'zoo'
##
## The following objects are masked from 'package:base':
##
## as.Date, as.Date.numeric

data.dir <- "/Users/McInnis/MEGA/Exeter/Courses/Term2/Financial Econometrics/Lab/Data for labs/"
data.filename <- "crsp-monthly-ret-31Dec1925-31Dec2013.csv"
data.filepath <- paste(data.dir, data.filename, sep="")
crsp.data <- read.table(data.filepath, header=TRUE, sep=",")

vwretd = crsp.data[[2]]
vwretx = crsp.data[[3]]
dp2.t <- (1 + vwretd)/(1+vwretx) - 1

# Create Rtp1 and D-P for annual data and fit linear model
R.tp1 <- 1 + vwretd[-1]
dp.t <- (1 + vwretd)/(1+vwretx) - 1
dp.t <- dp.t[-length(dp.t)]
ret1reg <- lm(R.tp1 ~ dp.t)
summary(ret1reg)

# Correct standard errors
cse.ret1reg <- vcovHAC(ret1reg, weights=c(1,1,1), adjust=FALSE)
sqrt(diag(cse.ret1reg))


# Two year return regressions
R.tp2 <- c(rollapply(1 + vwretd[2:length(vwretd)], width=2, prod), NA*1:1)
ret2reg <- lm(R.tp2 ~ dp.t)
summary(ret2reg)

# Correct standard errors
cse.ret2reg <- vcovHAC(ret2reg, weights=c(1,1,1), adjust=FALSE)
sqrt(diag(cse.ret2reg))

# Three year return regressions
R.tp3 <- c(rollapply(1 + vwretd[2:length(vwretd)], width=3, prod), NA*1:2)
ret3reg <- lm(R.tp3 ~ dp.t)
summary(ret3reg)

# Correct standard errors
cse.ret3reg <- vcovHAC(ret3reg, weights=c(1,1,1), adjust=FALSE)
sqrt(diag(cse.ret3reg))


# Three year return regressions
R.tp30 <- c(rollapply(1 + vwretd[2:length(vwretd)], width=30, prod), NA*1:29)
ret30reg <- lm(R.tp30 ~ dp.t)
summary(ret30reg)

# Correct standard errors
cse.ret30reg <- vcovHAC(ret30reg, weights=c(1,1,1), adjust=FALSE)
sqrt(diag(cse.ret30reg))


fn_return_predic <- function(vwretd, dp.t, n, is_correct = NULL){
  ## gross return
  if (n == 1) {
    R.tpx <- 1 + vwretd[-1]
  } else {
    R.tpx <- c(rollapply(1 + vwretd[2:length(vwretd)], width=n, prod), NA*1:(n-1))
  }
  
  dp.t = dp.t[-length(dp.t)]

  retreg <- lm(R.tpx ~ dp.t)
  if(is_correct){
    # Correct standard errors"
    #cse.retreg <- vcovHAC(retreg, weights=c(1,1,1), adjust=FALSE)
    cse.retreg <- vcovHAC(retreg)
    sqrt(diag(cse.retreg))[2]
  }
  else {
    summary(retreg)$coefficients[,1][2]
  }
  
}

periods = 1:100
beta = sapply(periods, function(x, y, z, w) fn_return_predic(y, w, x, z), y=vwretd, w=dp2.t, z=FALSE)  
plot(periods, beta, type="l", lwd=2, 
     xlab="Horizons of cont. compounded return", ylab="Estimate, b", main="Rt->k = a + b*dp", col= "red" )
beta


periods = 1:100
beta = sapply(periods, function(x, y, z, w) fn_return_predic(y, w, x, z), y=vwretd, w=dp2.t, z=TRUE)  
plot(periods, beta, type="l", lwd=2, 
     xlab="Horizons of cont. compounded return", ylab="Estimate, b", main="Rt->k = a + b*dp", col= "red" )
beta

