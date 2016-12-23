
rm(list=ls())
library(zoo)
library(sandwich)

IDX_DATE = 1
IDX_CLOSE_PRICE = 5

IDX_TBILL_RATE = 2

options(digits=10, width=70)
source("/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/Code/my_func.R")

strReverse <- function(x) sapply(lapply(strsplit(x, NULL), rev), paste, 
                                 collapse="") 

## prices for months and years ##

dir <- "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/"
table.data <- my_open(dir, "yahoo.twii.csv")

table.date = as.character(table.data[[IDX_DATE]])
table.price = table.data[[IDX_CLOSE_PRICE]]

table.year = sapply(table.date, function(x) substr(x, start=7, stop=10))
table.year.levels = levels(factor(table.year, ordered = TRUE))
table.year.prices = sapply(table.year.levels, function(x, y, z) (z[y == x, IDX_CLOSE_PRICE])[1], y=table.year, z=table.data)
write.table(table.year.prices, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/price-year.csv", sep = ",", col.names = NA, qmethod = "double")

table.month = sapply(table.date, function(x) substr(x, start=4, stop=10))
table.month = strsplit(table.month, split="/")
table.month = sapply(1:length(table.month), function(x) paste(rev(table.month[[x]]),collapse = ""))
table.month.levels = levels(factor(table.month, ordered = TRUE))
table.month.prices = sapply(table.month.levels, function(x, y, z) (z[y == x, IDX_CLOSE_PRICE])[1], y=table.month, z=table.data)
write.table(table.month.prices, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/price-month.csv", sep = ",", col.names = NA, qmethod = "double")

## t-bill for months and years ##

dir <- "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/"
table.data <- my_open(dir, "t-bill-ten-year.csv")

table.date = as.character(table.data[[IDX_DATE]])
table.price = table.data[[IDX_TBILL_RATE]]

#table.year = sapply(table.date, function(x) as.numeric(strsplit(x, "/")[[1]][1]))
table.year = sapply(table.date, function(x) substr(x, start=1, stop=3))
table.year.levels = levels(factor(table.year, ordered = TRUE))
table.year.tbill = sapply(table.year.levels, function(x, y, z) (z[y == x, IDX_TBILL_RATE])[1], y=table.year, z=table.data)
write.table(table.year.tbill, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/tbill-year.csv", sep = ",", col.names = NA, qmethod = "double")

table.month = sapply(table.date, function(x) substr(x, start=1, stop=6))
#table.month = strsplit(table.month, split="/")
#table.month = sapply(1:length(table.month), function(x) paste(rev(table.month[[x]]),collapse = ""))
table.month.levels = levels(factor(table.month, ordered = TRUE))
table.month.tbill = sapply(table.month.levels, function(x, y, z) (z[y == x, IDX_TBILL_RATE])[1], y=table.month, z=table.data)
write.table(table.month.tbill, file = "/Users/McInnis/Dropbox/Exeter/Courses/Dissertation/Data Processing/tbill-month.csv", sep = ",", col.names = NA, qmethod = "double")

