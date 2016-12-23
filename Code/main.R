

rm(list=ls())
library(zoo)
library(sandwich)

options(digits=4, width=70)
source("/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/Code/my_func.R")
source("/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/Code/my_log_func.R")
source("/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/Code/my_basic.R")

CONT_FINANCIAL_CRISIS <- 69
CONT_BEGINING_RECOVER <- 71

#: open monthly csv of taiwan stock
dir <- "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/"
sp.data <- my_open(dir, "taiwan-stock-monthly-data.csv")

sp.bc.data <- sp.data[1:(CONT_FINANCIAL_CRISIS-1),]
sp.ac.data <- sp.data[(CONT_BEGINING_RECOVER):length(sp.data),]


my_regression(sp.data, IDX_AUTOREGRESS_DP)
my_regression(sp.data, IDX_REGR_RETP1_DP)


my_return_predict_plot_periods(sp.data, 1, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.data, 2, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.data, 6, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.data, 12, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.data, 24, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.data, 30, is_correct=FALSE, mode = MOD_GROSS_RETURN)

my_return_predict_plot_periods(sp.bc.data, 1:30, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.data, 1:40, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.ac.data, 1:30, is_correct=FALSE, mode = MOD_GROSS_RETURN)

my_return_predict_plot_periods(sp.ac.data, 1, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.ac.data, 2, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.ac.data, 6, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.ac.data, 12, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.ac.data, 24, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.ac.data, 30, is_correct=FALSE, mode = MOD_GROSS_RETURN)

my_return_predict_plot_periods(sp.bc.data, 1, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.bc.data, 2, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.bc.data, 6, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.bc.data, 12, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.bc.data, 24, is_correct=FALSE, mode = MOD_GROSS_RETURN)
my_return_predict_plot_periods(sp.bc.data, 30, is_correct=FALSE, mode = MOD_GROSS_RETURN)


# my_return_predict_plot_periods(sp.data, 1:30, is_correct=FALSE, mode = MOD_GROSS_RETURN)
# my_return_predict_plot_periods(sp.bc.data, 1:30, is_correct=FALSE, mode = MOD_GROSS_RETURN)
# my_return_predict_plot_periods(sp.ac.data, 1:30, is_correct=FALSE, mode = MOD_GROSS_RETURN)
# my_return_predict_plot_periods(sp.data, 1:100, is_correct=FALSE, mode = MOD_GROSS_RETURN)
# 
# 
# my_return_predict_plot_periods(sp.data, 30, is_correct=FALSE, mode = MOD_GROSS_RETURN)
# my_return_predict_plot_periods(sp.bc.data, 30, is_correct=FALSE, mode = MOD_GROSS_RETURN)


# yearly.data <- my_open(dir, "taiwan-stock-yearly-data.csv") 
# yearly.vwretd <- my_get_vwretd(yearly.data) 
# 
# my_return_predict_plot_periods(yearly.data[1:10,], 1:3, is_correct=FALSE, mode = MOD_GROSS_RETURN)
# my_return_predict_plot_periods(yearly.data, 1:5, is_correct=TRUE, mode = MOD_GROSS_RETURN)



#my_return_predict_plot_periods(monthly.data, 88, is_correct=FALSE, mode = MOD_NET_RETURN)
#my_coe_ret8div_dp(monthly.data, FALSE)
#my_log_cs_identity(sp.data)


# dir <- "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/"
# test.data <- my_open(dir, "crsp-monthly-ret-31Dec1925-31Dec2013.csv")
# test.vwretd <- test.data["vwretd"][-1,]



##: Regress gross return of time t+1 on gross return of time t
## Doc: (lab 2) The current gross return has somehow impact on next gross return

# monthly.vwR <- 1 + monthly.vwretd[-c(1, length(monthly.vwretd))]
# monthly.vwRtp1 <- 1 + monthly.vwretd[-c(1:2)]
# monthly.retreg <- lm(monthly.vwRtp1 ~ monthly.vwR)
# summary(monthly.retreg)
# monthly.retreg.res <- residuals(monthly.retreg)
# plot(monthly.retreg.res)
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.6895     0.0802    8.60  9.4e-15 ***
#   monthly.vwR   0.3429     0.0763    4.49  1.4e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# yearly.vwR <- 1 + yearly.vwretd[-c(1, length(yearly.vwretd))]
# yearly.vwRtp1 <- 1 + yearly.vwretd[-c(1:2)]
# yearly.retreg <- lm(yearly.vwRtp1 ~ yearly.vwR)
# summary(yearly.retreg)
# yearly.retreg.res <- residuals(yearly.retreg)
# plot(yearly.retreg.res)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    1.257      0.295    4.26  0.00093 ***
#   yearly.vwR    -0.177      0.272   -0.65  0.52673    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


##: Caculate autocorrelation upto seven lags for monthly and yearly net return data
## Doc: price sticky problem
##      in monthly result, lag one has somehow correlation with coefficient 0.346
##      in other periods and lags, the coefficient is close to zero

# monthly.vwretd.ac <- acf(monthly.vwretd, lag.max=7, na.action=na.pass, plot=FALSE)
# monthly.vwretx.ac <- acf(monthly.vwretx, lag.max=7, na.action=na.pass, plot=FALSE)
# Autocorrelations of series ‘monthly.vwretd’, by lag
# 
# 0      1      2      3      4      5      6      7 
# 1.000  0.346  0.024  0.061 -0.068 -0.101 -0.159 -0.144
#
# yearly.vwretd.ac <- acf(yearly.vwretd, lag.max=7, na.action=na.pass, plot=FALSE)
# yearly.vwretx.ac <- acf(yearly.vwretx, lag.max=7, na.action=na.pass, plot=FALSE)
#
# Autocorrelations of series ‘yearly.vwretx’, by lag
# 
# 0      1      2      3      4      5      6      7 
# 1.000 -0.183 -0.098  0.062  0.095 -0.279 -0.181  0.227 


##: caculate the return of sequent n periods with option of annulization
## Doc: w/o annulization, the return becomes larger when the subsequent of years grow

# r.sub1 = my_n_period_net_ret(monthly.data, 1, is_annual=FALSE, is_plot=TRUE)
# r.sub2 = my_n_period_net_ret(monthly.data, 2, is_annual=FALSE, is_plot=TRUE)
# r.sub3 = my_n_period_net_ret(monthly.data, 3, is_annual=FALSE, is_plot=TRUE)
# r.sub4 = my_n_period_net_ret(monthly.data, 4, is_annual=FALSE, is_plot=TRUE)




