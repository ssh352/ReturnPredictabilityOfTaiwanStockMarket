rm(list=ls())
options(digits=4, width=70)

source("/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/Code/my_basic.R")
library(urca)

dir <- "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/"
stock.data <- my_open(dir, "taiwan-stock-monthly-data.csv")

vwretd = my_get_vwretd(stock.data)
# Create Rtp1 and D-P for annual data and fit linear model
R.t <- 1 + vwretd
P.t <- my_get_price(stock.data)
D.t <- my_get_dividend(stock.data)

PP.t <- my_price_grow(stock.data)
DP.t <- my_dp_ratio(stock.data)
DD.t <- my_dividend_grow(stock.data)
TB.t <- 1 + my_tbill(stock.data)

# Log-linearization 
r.t <- log(R.t)
p.t <- log(P.t)
d.t <- log(D.t)

dp.t <- log(DP.t)
pp.t <- log(PP.t)
dd.t <- log(DD.t)
tb.t <- log(TB.t)

t.t <- dd.t - tb.t


p.t.pp <- PP.test(p.t)
d.t.pp <- PP.test(d.t)
pp.t.pp <- PP.test(pp.t)
dd.t.pp <- PP.test(dd.t)
tb.t.pp <- PP.test(tb.t)
t.t.pp <- PP.test(t.t)
dp.t.pp <- PP.test(dp.t)

plot(1:length(p.t), p.t, type="l", lwd=2, xlab="sapmle", ylab="p.t", col= "red" )
p.t.adf <- ur.df(p.t, type = c("trend"), selectlags = "AIC")
plot(1:length(d.t), d.t, type="l", lwd=2, xlab="sapmle", ylab="d.t", col= "red" )
d.t.adf <- ur.df(d.t, type = c("trend"), selectlags = "AIC")
plot(1:length(pp.t), pp.t, type="l", lwd=2, xlab="sapmle", ylab="pp.t", col= "red" )
pp.t.adf <- ur.df(pp.t, type = c("trend"), selectlags = "AIC")
plot(1:length(dd.t), dd.t, type="l", lwd=2, xlab="sapmle", ylab="dd.t", col= "red" )
dd.t.adf <- ur.df(dd.t, type = c("trend"), selectlags = "AIC")
plot(1:length(tb.t), tb.t, type="l", lwd=2, xlab="sapmle", ylab="tb.t", col= "red" )
tb.t.adf <- ur.df(tb.t, type = c("trend"), selectlags = "AIC")
plot(1:length(t.t), t.t, type="l", lwd=2, xlab="sapmle", ylab="t.t", col= "red" )
t.t.adf <- ur.df(t.t, type = c("trend"), selectlags = "AIC")
plot(1:length(dp.t), dp.t, type="l", lwd=2, xlab="sapmle", ylab="dp.t", col= "red" )
dp.t.adf <- ur.df(dp.t, type = c("trend"), selectlags = "AIC")
dp.t.adf2 <- ur.df(dp.t, type = c("drift"), selectlags = "AIC")

p.t.kpss <- ur.kpss(p.t, type = c("tau"))
d.t.kpss <- ur.kpss(d.t, type = c("tau"))
pp.t.kpss <- ur.kpss(pp.t, type = c("tau"))
dd.t.kpss <- ur.kpss(dd.t, type = c("tau"))
tb.t.kpss <- ur.kpss(tb.t, type = c("tau"))
t.t.kpss <- ur.kpss(t.t, type = c("tau"))
dp.t.kpss <- ur.kpss(dp.t, type = c("tau"))
dp.t.kpss2 <- ur.kpss(dp.t, type = c("mu"))


# write.table(p.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/p.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(d.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/d.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(pp.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/pp.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(dd.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/dd.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(tb.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/tb.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(dp.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/dp.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(DP.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/DP1.t.csv", sep = ",", col.names = NA, qmethod = "double")
# write.table(t.t, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/unit_root/t.t.csv", sep = ",", col.names = NA, qmethod = "double")



