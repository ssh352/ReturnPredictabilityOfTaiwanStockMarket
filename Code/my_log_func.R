

library(zoo)
library(sandwich)
library(expm)
library(vars)


CON_HORIZEN_INFINITY <- 0


my_log_cs_identity <- function(stock.data) {
  vwretd = my_get_vwretd(stock.data)
  # Create Rtp1 and D-P for annual data and fit linear model
  R.t <- 1 + vwretd
  DP.t <- my_dp_ratio(stock.data)
  DD.tp1 <- my_dividend_grow(stock.data)[-1]
  
  # Log-linearization 
  r.tp1 <- log(R.t)[-1]
  dp.t <- log(DP.t)[-length(DP.t)]
  dp.tp1 <- log(DP.t)[-1]
  dd.tp1 <- log(DD.tp1)
  
  # Compute rho
  rho <- 1/(1 + exp(mean(dp.t)))
  #k <- - log(rho) - (1 - rho)*log(1/rho - 1)
  # Compute log returns implied by the C-S identity and compare to realized returns
  rCS.tp1 <- dd.tp1 + dp.t - rho*dp.tp1
  #rCS.tp1 <- k + dd.tp1 + dp.t - rho*dp.tp1
  # Compute and plot the absolute value of the difference
  diff <- r.tp1 - rCS.tp1
  #print(diff)
  
  y.lim=c(min(min(rCS.tp1, na.rm=TRUE), min(r.tp1, na.rm=TRUE)), max(max(rCS.tp1, na.rm=TRUE), max(r.tp1, na.rm=TRUE)))
  
  plot(r.tp1, type="l", ylim=y.lim)
  lines(rCS.tp1, type="l", col="red")
  lines(diff, type="l", col="blue")
  
  # Correlation
  print("correlation of r.tp1 and rCS.tp1")
  print(cor.test(r.tp1, rCS.tp1))
  
  # Run OLS regressions
  # Return regression
  retreg <- lm(r.tp1 ~ dp.t)
  br.hat <- coef(retreg)[[2]]
  print(summary(retreg))
  
  # Dividend regression
  divreg <- lm(dd.tp1 ~ dp.t)
  bd.hat <- coef(divreg)[[2]]
  print(summary(divreg))
  
  # dp regression
  dpreg <- lm(dp.tp1 ~ dp.t)
  phi.hat <- coef(dpreg)[[2]]
  print(summary(dpreg))
  
  #Return forecastability coefficient
  br.hat.imp <- bd.hat + 1 - rho*phi.hat
  #print(br.hat - br.hat.imp)
  
  #Div-growth forecastability coefficient
  bd.hat.imp <- br.hat - 1 + rho*phi.hat
  #print(bd.hat - bd.hat.imp)
  
  #dp autocorrelation coefficient
  phi.hat.imp <- (bd.hat - br.hat + 1)/rho
  #print(phi.hat - phi.hat.imp)
  
  hat <- c(br.hat, bd.hat, phi.hat)
  names(hat) <- c("br.hat", "bd.hat", "phi.hat")
  print(hat)
  
  hat.imp <- c(br.hat.imp, bd.hat.imp, phi.hat.imp)
  names(hat.imp) <- c("br.imp", "bd.imp", "phi.imp")
  print(hat.imp)
  
  br.diff <- br.hat - br.hat.imp
  bd.diff <- bd.hat - bd.hat.imp
  phi.diff <- phi.hat - phi.hat.imp
  diff <- c(br.diff, bd.diff, phi.diff)
  names(diff) <- c("br.diff", "bd.diff", "phi.diff")
  print(diff)
}

my_log_coefs <- function(stock.data, is_correct) {
  vwretd = my_get_vwretd(stock.data)
  # Create Rtp1 and D-P for annual data and fit linear model
  R.t <- 1 + vwretd
  DP.t <- my_dp_ratio(stock.data)
  DD.tp1 <- my_dividend_grow(stock.data)[-1]
  
  # Log-linearization 
  r.tp1 <- log(R.t)[-1]
  dp.t <- log(DP.t)[-length(DP.t)]
  dp.tp1 <- log(DP.t)[-1]
  dd.tp1 <- log(DD.tp1)

  # Run OLS regressions
  # Return regression
  retreg <- lm(r.tp1 ~ dp.t)
  br.hat <- coef(retreg)[[2]]
  
  # Dividend regression
  divreg <- lm(dd.tp1 ~ dp.t)
  bd.hat <- coef(divreg)[[2]]
  
  # dp regression
  dpreg <- lm(dp.tp1 ~ dp.t)
  phi.hat <- coef(dpreg)[[2]]
  
  coefs <- c(br.hat, bd.hat, phi.hat)
  names(coefs) <- c("br.hat", "bd.hat", "phi.hat")
  coefs
}

my_log_var_2param <- function(dp.t, rd.t, A = NULL, adjust = 0) {
  rho <- 1/(1 + exp(mean(dp.t)))
  k <- 0 ##- log(rho) - (1 - rho)*log(1/rho - 1)
  c <- adjust
  
  #transform to deviation form
  dp.t = dp.t - mean(dp.t)
  rd.t= rd.t - mean(rd.t)
  
  data.ser <- cbind(dp.t, rd.t)
  my <- NULL
  if (is.null(A)){
    my$var <- VAR(data.ser, p=1, type="const")
    my$A <- rbind(coef(my$var)$dp.t[,1][1:2], coef(my$var)$rd.t[,1][1:2])
  } else {
    my$A <- A
  }
  
  rownames(my$A) <- c("dp.t", "rd.t")
  e2 = matrix(c(0, 1))
  my$A_brd <- t(e2) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A)
  my$A_dcterm <- 1 - my$A_brd
  my$coef <- t(e2) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A)
  e1 = matrix(c(1, 0))
  my$test <- (t(e1) %*% (diag(dim(my$A)[1]) - rho*my$A)) - (t(e2) %*% my$A)
  my$dp <- my$coef[1]*dp.t + my$coef[2]*rd.t + (c - k)/(1-rho)

  my$lm <- lm(my$dp~dp.t)
  my$lm_dp_rd <- lm(my$dp~dp.t+rd.t)
  my$cor <- cor.test(my$dp, dp.t)
  my$wald <- wald.test(b=coef(my$lm), Sigma=vcov(my$lm), Terms=2, H0=c(1), df=1)
  my$sd_ratio <- sd(my$dp)/sd(dp.t)
  my$rho <- rho
  my$org_dp <- dp.t
  
  y.lim=c(min(min(my$dp, na.rm=TRUE), min(dp.t, na.rm=TRUE)), max(max(my$dp, na.rm=TRUE), max(dp.t, na.rm=TRUE)))
  plot(1:length(dp.t), dp.t, type="l", lwd=2, 
       xlab="samples", ylab="dp.t", ylim=y.lim, main="E(dp.t) vs dp.t", col= "red" )
  lines(1:length(my$dp), my$dp, lwd=2, col="blue")
  legend("topright", c("dp.t", "E(pd.t)"), lty=c(1,1), col=c('red', 'blue'))
  
  my
}

my_log_var_3param <- function(dp.t, dd.t, r.t, A = NULL, adjust = 0) {
  rho <- 1/(1 + exp(mean(dp.t)))
  k <- 0 ##- log(rho) - (1 - rho)*log(1/rho - 1)
  c <- adjust
  
  #transform to deviation form
  dp.t = dp.t - mean(dp.t)
  dd.t = dd.t - mean(dd.t)
  r.t = r.t - mean(r.t)
  
  data.ser <- cbind(dp.t, dd.t, r.t)
  my <- NULL
  if (is.null(A)){
    my$var <- VAR(data.ser, p=1, type="const")
    my$A <- rbind(coef(my$var)$dp.t[,1][1:3], coef(my$var)$dd.t[,1][1:3], coef(my$var)$r.t[,1][1:3])
  } else {
    my$A <- A
  }
  
  rownames(my$A) <- c("dp.t", "dd.t", "r.t")
  e2 = matrix(c(0, 1, 0))
  e3 = matrix(c(0, 0, 1))
  #my$coef <- t(e3) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A) - t(e2) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A)
  my$coef <- (t(e3) - t(e2)) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A)
  e1 = matrix(c(1, 0, 0))
  my$A_br <- t(e3) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A)
  my$A_bd <- t(e2) %*% my$A %*% solve(diag(dim(my$A)[1]) - rho*my$A)
  my$test <- (t(e1) %*% (diag(dim(my$A)[1]) - rho*my$A)) - ((t(e3) - t(e2)) %*% my$A)
  my$dp <- my$coef[1]*dp.t + my$coef[2]*dd.t + my$coef[3]*r.t + (c - k)/(1-rho)
  my$r.tp1 <- my$A_br[1]*dp.t + my$A_br[2]*dd.t + my$A_br[3]*r.t + (c - k)/(1-rho)
  my$dd.tp1 <- my$A_bd[1]*dp.t + my$A_bd[2]*dd.t + my$A_bd[3]*r.t + (c - k)/(1-rho)
  my$dcterm <- my$dp-my$r.tp1+my$dd.tp1
  my$lm <- lm(my$dp~dp.t)
  my$lm_br <- lm(my$r.tp1~dp.t)
  my$lm_bd <- lm(my$dd.tp1~dp.t)
  #my$lm_dcterm <- 1-(coef(my$lm_br)-coef(my$lm_bd))
  my$lm_dcterm <- lm(my$dcterm~dp.t)
  my$lm_br_bd <- lm(my$dp~dp.t+my$r.tp1+my$dd.tp1)
  
  my$e_r = my$dp-my$dd.tp1
  my$e_r_sd_ratio = sd(my$e_r)/sd(dp.t)
  my$e_v_sd_ratio = sd(my$r.tp1)/sd(dp.t)
  my$e_cor = cor.test(my$e_r, my$r.tp1)
  
  my$e_d_sd_ratio = sd(my$dd.tp1)/sd(dp.t)
  my$e_d_cor = cor.test(-my$dd.tp1, dp.t)
  
  my$cor <- cor.test(dp.t, my$dp)
  my$wald <- wald.test(b=coef(my$lm), Sigma=vcov(my$lm), Terms=2, H0=c(1), df=1)
  my$sd_ratio <- sd(my$dp)/sd(dp.t)
  my$rho <- rho
  
  my$org_dp <- dp.t
  
  y.lim=c(min(min(my$dp, na.rm=TRUE), min(dp.t, na.rm=TRUE)), max(max(my$dp, na.rm=TRUE), max(dp.t, na.rm=TRUE)))
  plot(1:length(dp.t), dp.t, type="l", lwd=2, 
       xlab="samples", ylab="dp.t", ylim=y.lim, main="E(dp.t) vs dp.t", col= "red" )
  lines(1:length(my$dp), my$dp, lwd=2, col="blue")
  legend("topright", c("dp.t", "E(pd.t)"), lty=c(1,1), col=c('red', 'blue'))
  
  my
}

my_log_var_multiperi_2param <- function(dp.t, rd.t, mp = 1, A = NULL, adjust = 0){
  rho <- 1/(1 + exp(mean(dp.t)))
  k <- 0 ##- log(rho) - (1 - rho)*log(1/rho - 1)
  c = adjust
  e2 = matrix(c(0, 1))
  
  #transform to deviation form
  dp.t = dp.t - mean(dp.t)
  rd.t = rd.t - mean(rd.t)
  
  fn_dp_expect <- function (p, A, rho, dp.t, r.t, seq) {
    z.t = matrix(c(dp.t[seq], r.t[seq]))

    ## Note: rho^p * E(dp.tpi) ~= rho^p * dp.tpi
    ret <- t(e2) %*% A %*% z.t + (rho^p) * dp.t[seq + p] + (c - k)*(1 - rho^p)/(1-rho)
  }
  
  fn_r_expect <- function (p, A, rho, dp.t, r.t, seq) {
    z.t = matrix(c(dp.t[seq], r.t[seq]))
    
    ## Note: rho^p * E(dp.tpi) ~= rho^p * dp.tpi
    ret <- t(e2) %*% A %*% z.t
  }
  
  data.ser <- cbind(dp.t, rd.t)
  
  my <- NULL
  if (is.null(A)){
    my$var <- VAR(data.ser, p=1, type="const")
    my$A <- rbind(coef(my$var)$dp.t[,1][1:2], coef(my$var)$rd.t[,1][1:2])
  } else {
    my$A <- A
  }
  rownames(my$A) <- c("dp.t", "rd.t")

  aggrmat <- 0
  for(i in 1:mp){
    aggrmat <- aggrmat + (rho^(i-1)) * (my$A %^% i)
  }
  my$dp <- sapply(1:(length(dp.t)-mp), function(x, y, z, m, n, w) fn_dp_expect(y, z, w, m, n, x), 
                    y=mp, z=aggrmat, w=rho, m=dp.t, n=rd.t)
  
  my$rd.tp1 <- sapply(1:(length(dp.t)-mp), function(x, y, z, m, n, w) fn_r_expect(y, z, w, m, n, x), 
                  y=mp, z=aggrmat, w=rho, m=dp.t, n=rd.t)

  dp2.t <- dp.t[-c((length(dp.t)-mp+1):length(dp.t))]
  rd2.t <- rd.t[-c((length(rd.t)-mp+1):length(rd.t))]
  my$dcterm <- (rho^mp * dp.t)[-c(1:mp)]
  
  my$A_brd <- t(e2) %*% aggrmat
  my$A_dcterm <- 1 - my$A_brd
  my$coef <- t(e2) %*% aggrmat
  my$lm <- lm(my$dp ~ dp2.t)
  my$lm_brd <- lm(my$rd.tp1 ~ dp2.t)
  my$lm_dcterm <- lm(my$dcterm ~ dp2.t)
  my$lm_dp_rd <- lm(my$dp~dp2.t+rd2.t)
  my$cor <- cor.test(dp2.t, my$dp)
  my$wald <- wald.test(b=coef(my$lm), Sigma=vcov(my$lm), Terms=2, H0=c(1), df=1)
  my$sd_ratio <- sd(my$dp)/sd(dp2.t)
  my$rho <- rho
  my$diff <- mean(dp2.t-my$dp)
  
  y.lim=c(min(min(my$dp, na.rm=TRUE), min(dp.t, na.rm=TRUE)), max(max(my$dp, na.rm=TRUE), max(dp.t, na.rm=TRUE)))
  plot(1:length(dp.t), dp.t, type="l", lwd=2, 
       xlab="samples", ylab="dp.t", ylim=y.lim, main="E(dp.t) vs dp.t", col= "red" )
  lines(1:length(my$dp), my$dp, lwd=2, col="blue")
  legend("topright", c("dp.t", "E(pd.t)"), lty=c(1,1), col=c('red', 'blue'))
  
  my
}

my_log_var_multiperi_3param <- function(dp.t, dd.t, r.t, mp = 1, A = NULL, adjust = 0){
  rho <- 1/(1 + exp(mean(dp.t)))
  k <- 0 ##- log(rho) - (1 - rho)*log(1/rho - 1)
  c = adjust
  e2 = matrix(c(0, 1, 0))
  e3 = matrix(c(0, 0, 1))
  
  #transform to deviation form
  dp.t = dp.t - mean(dp.t)
  dd.t = dd.t - mean(dd.t)
  r.t = r.t - mean(r.t)
  
  fn_dp_expect <- function (p, A, rho, dp.t, dd.t, r.t, seq) {
    z.t = matrix(c(dp.t[seq], dd.t[seq], r.t[seq]))
    
    ## Note: rho^p * E(dp.tpi) ~= rho^p * dp.tpi
    ret <- (t(e3) - t(e2)) %*% A %*% z.t + (rho^p) * dp.t[seq + p] + (c - k)*(1 - rho^p)/(1-rho)
  }
  
  fn_r_expect <- function (p, A, rho, dp.t, dd.t, r.t, seq) {
    z.t = matrix(c(dp.t[seq], dd.t[seq], r.t[seq]))
    
    ## Note: rho^p * E(dp.tpi) ~= rho^p * dp.tpi
    ret <- t(e3) %*% A %*% z.t
  }
  
  fn_dd_expect <- function (p, A, rho, dp.t, dd.t, r.t, seq) {
    z.t = matrix(c(dp.t[seq], dd.t[seq], r.t[seq]))
    
    ## Note: rho^p * E(dp.tpi) ~= rho^p * dp.tpi
    ret <- t(e2) %*% A %*% z.t
  }
  
  data.ser <- cbind(dp.t, dd.t, r.t)
  my <- NULL
  if (is.null(A)){
    my$var <- VAR(data.ser, p=1, type="const")
    my$A <- rbind(coef(my$var)$dp.t[,1][1:3], coef(my$var)$dd.t[,1][1:3], coef(my$var)$r.t[,1][1:3])
  } else {
    my$A <- A
  }
  rownames(my$A) <- c("dp.t", "dd.t", "r.t")
  
  aggrmat <- 0
  for(i in 1:mp){
    aggrmat <- aggrmat + (rho^(i-1)) * (my$A %^% i)
  }
  my$dp <- sapply(1:(length(dp.t)-mp), function(x, y, z, m, n, s, w) fn_dp_expect(y, z, w, m, n, s, x), 
                  y=mp, z=aggrmat, w=rho, m=dp.t, n=dd.t, s=r.t)
  
  my$r.tp1 <- sapply(1:(length(dp.t)-mp), function(x, y, z, m, n, s, w) fn_r_expect(y, z, w, m, n, s, x), 
                  y=mp, z=aggrmat, w=rho, m=dp.t, n=dd.t, s=r.t)
  
  my$dd.tp1 <- sapply(1:(length(dp.t)-mp), function(x, y, z, m, n, s, w) fn_dd_expect(y, z, w, m, n, s, x), 
                      y=mp, z=aggrmat, w=rho, m=dp.t, n=dd.t, s=r.t)

  my$dcterm <- (rho^mp * dp.t)[-c(1:mp)]
  dp2.t <- dp.t[-c((length(dp.t)-mp+1):length(dp.t))]
  
  my$A_br <- t(e3) %*% aggrmat
  my$A_bd <- t(e2) %*% aggrmat
  my$A_dcterm <- 1-(my$A_br-my$A_bd)
  my$coef <- (t(e3) - t(e2)) %*% aggrmat
  my$lm <- lm(my$dp ~ dp2.t)
  my$lm_br <- lm(my$r.tp1 ~ dp2.t)
  my$lm_bd <- lm(my$dd.tp1 ~ dp2.t)
  #my$lm_dcterm <- 1-(coef(my$lm_br)-coef(my$lm_bd))
  my$lm_dcterm <- lm(my$dcterm ~ dp2.t)
  
  my$e_r = my$dp-my$dd.tp1
  my$e_r_sd_ratio = sd(my$e_r)/sd(dp2.t)
  my$e_v_sd_ratio = sd(my$r.tp1)/sd(dp2.t)
  my$e_r_cor = cor.test(my$e_r, my$r.tp1)
  
  my$e_d_sd_ratio = sd(my$dd.tp1)/sd(dp2.t)
  my$e_d_cor = cor.test(-my$dd.tp1, dp2.t)
  
  my$cor <- cor.test(dp2.t, my$dp)
  my$wald <- wald.test(b=coef(my$lm), Sigma=vcov(my$lm), Terms=2, H0=c(1), df=1)
  my$sd_ratio <- sd(my$dp)/sd(dp2.t)
  my$rho <- rho
  my$diff <- mean(dp2.t-my$dp)
  
  y.lim=c(min(min(my$dp, na.rm=TRUE), min(dp.t, na.rm=TRUE)), max(max(my$dp, na.rm=TRUE), max(dp.t, na.rm=TRUE)))
  plot(1:length(dp.t), dp.t, type="l", lwd=2, 
       xlab="samples", ylab="dp.t", ylim=y.lim, main="E(dp.t) vs dp.t", col= "red" )
  lines(1:length(my$dp), my$dp, lwd=2, col="blue")
  legend("topright", c("dp.t", "E(pd.t)"), lty=c(1,1), col=c('red', 'blue'))
  
  my
}

my_log_multiperi_dicount_rho_coef <- function(dp.t, dd.t, r.t, mp = 1){
  rho <- 1/(1 + exp(mean(dp.t)))
  rhos <- rho^c(0:(mp-1))
  
  #transform to deviation form
  dp.t = dp.t - mean(dp.t)
  dd.t = dd.t - mean(dd.t)
  r.t = r.t - mean(r.t)
  
  dd.tp1 = dd.t[-1]
  r.tp1 = r.t[-1]
  dp.t = dp.t[-length(dp.t)]
  
  fn_sum_discount_value <- function (var, mp, seq) {
    ret = sum(var[seq:(seq+mp-1)]*rhos)
  }

  my <- NULL
  dp2.t = dp.t[-c((length(dp.t)-mp+1):length(dp.t))]
  my$dd.tp1 <- sapply(1:(length(dd.tp1)-mp), function(x, y, w) fn_sum_discount_value(y, w, x), 
                  y=dd.tp1, w=mp)
  my$r.tp1 <- sapply(1:(length(r.tp1)-mp), function(x, y, w) fn_sum_discount_value(y, w, x), 
                  y=r.tp1, w=mp)
  my$dcterm <- (rho^mp * dp.t)[-c(1:mp)]
  my$dp.t <- my$r.tp1 - my$dd.tp1 + my$dcterm
  
  my$lm <- lm(my$dp.t~dp2.t)
  my$lm_bd <- lm(my$dd.tp1~my$dp.t)
  my$lm_br <- lm(my$r.tp1~my$dp.t)
  my$lm_dcterm <- lm(my$dcterm~my$dp.t)
  my$wald <- wald.test(b=coef(my$lm), Sigma=vcov(my$lm), Terms=2, H0=c(1), df=1)
  
  my
}

