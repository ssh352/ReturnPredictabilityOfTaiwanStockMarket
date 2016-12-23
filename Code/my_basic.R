## data index in the sheet
IDX_DIVIDEND_PRICE_RATIO <- 14
IDX_AVERAGE_PRICE <- 10
IDX_TERMINAL_PRICE <- 11
IDX_PERIOD <- 1
IDX_TEN_YEAR_TBILL <- 17

## index for regression result
IDX_GET_DP = "dp.t"

## flag for funtion
BOOL_WO_CORRECT <- FALSE
BOOL_CORRECT <- TRUE

## mode for function operation
MOD_EXCESS_TBILL <- 1
MOD_GROSS_RETURN <- 2
MOD_NET_RETURN <- 3

my_open <- function(dir, fn){
  path.dir <- dir 
  path.filename <- fn
  path.filepath <- paste(path.dir, path.filename, sep="")
  read.table(path.filepath, header=TRUE, sep=",")
}

my_get_vwretd <- function(tw_data){
  dp <- tw_data[[IDX_DIVIDEND_PRICE_RATIO]]/100
  p <- tw_data[[IDX_AVERAGE_PRICE]]
  d <-  dp * p
  p <- tw_data[[IDX_TERMINAL_PRICE]]
  vwretd <- (p + d)[-1]/p[-length(p)] - 1
}

my_get_vwretx <- function(tw_data){
  p <- tw_data[[TDX_TERMIANL_PRICE]]
  vwretx <- p[-1]/p[-length(p)] - 1
}

my_dp_ratio <- function(tw_data){
  #dp <- tw_data[[IDX_DIVIDEND_PRICE_RATIO]][-1]/100
  dp <- tw_data[[IDX_DIVIDEND_PRICE_RATIO]]/100
  p <- tw_data[[IDX_AVERAGE_PRICE]]
  d <-  dp * p
  p <- tw_data[[IDX_TERMINAL_PRICE]]
  (d/p)[-1]
}

my_tbill <- function(tw_data){
  tbill <- tw_data[[IDX_TEN_YEAR_TBILL]]
  tbill[-1]
}

my_dividend_grow <- function(tw_data) {
  dp <- tw_data[[IDX_DIVIDEND_PRICE_RATIO]]/100
  p <- tw_data[[IDX_AVERAGE_PRICE]]
  d <-  dp * p
  d[-1]/d[-length(d)]
}

my_get_price <- function(tw_data){
  p <- tw_data[[IDX_TERMINAL_PRICE]][-1]
}

my_get_dividend <- function(tw_data){
  dp <- tw_data[[IDX_DIVIDEND_PRICE_RATIO]]/100
  p <- tw_data[[IDX_AVERAGE_PRICE]]
  d <-  (dp * p)[-1]
}

my_price_grow<- function(tw_data){
  p <- tw_data[[IDX_TERMINAL_PRICE]]
  pp <- p[-1]/p[-length(p)]
}

