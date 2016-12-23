library(zoo)
library(sandwich)

source("/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/Code/my_basic.R")

## data index in the sheet
IDX_AUTOREGRESS_DP <- 1
IDX_REGR_RETP1_DP <-2 

my_n_period_gross_return <- function (tw_data, n, is_annual=FALSE, is_plot=FALSE){
  net_ret = my_get_vwretd(tw_data)
  #dp = tw_data[[IDX_DIVIDEND_PRICE_RATIO]]/100
  dp = my_dp_ratio(tw_data)
  period = 2:length(tw_data[[IDX_PERIOD]])
  #xlabels = c(tw_data[[IDX_PERIOD]])
  gr = net_ret + 1
  
  if(is_annual == TRUE){
    main_str = sprintf("D/P and %d period subsequent annualized returns", n);
    anpr = (c(rollapply(gr[2:length(gr)], width=n, prod), NA*1:n))^(1/n) - 1
    #y.lim=c(-0.3, 0.3)
  } else {
    main_str = sprintf("D/P and %d period subsequent returns", n);
    anpr = (c(rollapply(gr[2:length(gr)], width=n, prod), NA*1:n)) - 1
    #y.lim=c(-0.5, 1)
  }
  
  y.lim=c(min(min(anpr, na.rm=TRUE), min(dp, na.rm=TRUE)), max(max(anpr, na.rm=TRUE), max(dp, na.rm=TRUE)))
  #x.lim=c(min(period), max(period))
  if(is_plot == TRUE){
    plot(period, anpr, type="l", lwd=2, 
         ##side=1, at=period, labels=xlabels,
         ##xaxt="n",
         xlab="", ylab="", ylim=y.lim, main=main_str, col= "red" )
    ##axis(side=1, at=period,labels=xlabels)
    lines(period, dp, lwd=2, col="blue")
    sub_str = sprintf("Sub. %dpds return", n)
    legend("topright", c(sub_str, "D/P"), lty=c(1,1), col=c("red", "blue"))
  }
  
  anpr
}

inner_my_return_predict <- function(tw_data, n, mode = MOD_GROSS_RETURN){
  vwretd <- my_get_vwretd(tw_data)
  
  ## gross return
  if (n == 1) {
    R.tpx <- 1 + vwretd[-1]
  } else {
    R.tpx <- c(rollapply(1 + vwretd[2:length(vwretd)], width=n, prod), NA*1:(n-1))
  }
  
  ## excess return, if sepcify the mode
  if (mode == MOD_NET_RETURN) {
    bm.tpx <- 1
  } else if (mode == MOD_EXCESS_TBILL) {
    tbill <- my_tbill(tw_data)
    
    if (n == 1) {
      bm.tpx <- 1 + tbill[-1] 
    } else {
      bm.tpx <- c(rollapply(1 + tbill[2:length(vwretd)], width=n, prod), NA*1:(n-1))
    }
  } else {
    bm.tpx <- 0
  }
    
  ## final excess return
  R.tpx = R.tpx - bm.tpx
  
  dp.t = my_dp_ratio(tw_data)
  dp.t = dp.t[-length(dp.t)]
  
  print("+++++++++++++++++++")
  print(R.tpx)
  print(dp.t)
  print("-------------------")  
  
  retreg <- lm(R.tpx ~ dp.t)
  print(summary(retreg))
  retreg
}

my_return_predict <- function(stock.data, n, is_correct = TRUE, mode = MOD_GROSS_RETURN){
  retreg = inner_my_return_predict(stock.data, n, mode)
  
  if(is_correct){
    # Correct standard errors"
    cse.retreg <- vcovHAC(retreg, weights=c(0, 1), adjust=FALSE)
    # cse.retreg <- vcovHAC(retreg, adjust=FALSE)
    summary(sqrt(diag(cse.retreg)))
    
    sqrt(diag(cse.retreg))
  }
  else {
    summary(retreg)$coefficients[,1]
  }
}

my_return_predict_coef <- function(stock.data, n, is_correct = TRUE, mode = MOD_GROSS_RETURN){
    my_return_predict(stock.data, n, is_correct, mode)[IDX_GET_DP]  
}

my_return_predict_plot_periods <- function(stock.data, periods, is_correct = TRUE, mode = MOD_GROSS_RETURN){
  beta = sapply(periods, function(x, y, z, k) my_return_predict_coef(y, x, z, k), y=stock.data, z=is_correct, k=mode)  
  names(beta) = periods
  
  plot(periods, beta, type="l", lwd=2, 
       xlab="Horizons of cont. compounded return", ylab="Estimate, b", main="Rt->k = a + b*dp", col= "red" )
  beta
}

my_coef_ret8div_dp <- function(stock.data, is_correct=FALSE){ 
  DP = my_dp_ratio(monthly.data) 
  dp.tp1 = log(DP[-1]) 
  dp.t = log(DP[-length(DP)]) 
  dp.estimate <- lm(dp.tp1 ~ dp.t) 
  print("===== regress dp.tp1 on dp.t =====") 
  print(dp.estimate) 
  if(is_correct) { 
    dp.cse <- vcovHAC(dp.estimate, weights=c(1,1), adjust=FALSE) 
    print(sqrt(diag(dp.cse))) 
  } 
  
  r.tp1 = log(1 + my_get_vwretd(monthly.data))[-1] 
  r.estimate = lm(r.tp1 ~ dp.t) 
  print("===== regress r.tp1 on dp.t =====") 
  print(r.estimate) 
  if(is_correct) { 
    r.cse <- vcovHAC(r.estimate, weights=c(1,1), adjust=FALSE) 
    print(sqrt(diag(r.cse))) 
  } 
  
  dd.tp1 = log(my_dividend_grow(monthly.data))[-1] 
  dd.estimate = lm(dd.tp1 ~ dp.t) 
  print("===== regress dd.tp1 on dp.t =====") 
  print(dd.estimate) 
  if(is_correct) { 
    dd.cse <- vcovHAC(dd.estimate, weights=c(1,1), adjust=FALSE) 
    print(sqrt(diag(dd.cse))) 
  } 
} 

my_regression <- function(sp.data, mode = IDX_AUTOREGRESS_DP) {
  if(mode == IDX_AUTOREGRESS_DP){
    dp = my_dp_ratio(sp.data)
    
    dp.tp1 = dp[-1]
    dp.t = dp[-length(dp)]
    retreg <- lm(dp.tp1 ~ dp.t)
    print(summary(retreg))
  } else if (mode == IDX_REGR_RETP1_DP) {
    inner_my_return_predict(sp.data, 1, MOD_GROSS_RETURN)
  }
}


