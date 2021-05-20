# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 4")
setwd("C:/Users/p_lae/OneDrive - Universit?t Z?rich UZH/Dokumente/Universit?t Z?rich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 3")

############
# Packages #
############
# uncomment the following lines to install the packages
 # install.packages("xts")
 # install.packages("PerformanceAnalytics")
 # install.packages("psych")
 # install.packages("roll")
 # install.packages("data.table")
###########

# load libraries
library(xts)
library(PerformanceAnalytics)
library(psych)
library(roll)
library(data.table)
library(ggplot2)


###############
# Data import #
###############

prices_adjusted <- read.delim(file = 'A4_dataset_01.txt', header = TRUE, sep = '\t', dec = '.')
book_values <- read.delim(file = 'A4_dataset_02.txt', header = TRUE, sep = '\t', dec = '.')
shares <- read.delim(file = 'A4_dataset_03.txt', header = TRUE, sep = '\t', dec = '.')
prices_unadjusted <- read.delim(file = 'A4_dataset_04.txt', header = TRUE, sep = '\t', dec = '.')
riskfree <- read.delim(file = 'A4_dataset_05.txt', header = TRUE, sep = '\t', dec = '.')
factor_returns <- read.delim(file = 'A4_dataset_06.txt', header = TRUE, sep = '\t', dec = '.')




prices_adjusted <- xts(prices_adjusted[,-1], order.by = as.Date(prices_adjusted $Date, format = "%d.%m.%Y"))
returns <- Return.calculate(prices = prices_adjusted, method = 'log')

book_values <- xts(book_values[,-1], order.by = as.Date(book_values$Date, format = "%d.%m.%Y"))

shares <- xts(shares[,-1], order.by = as.Date(shares$Date, format = "%d.%m.%Y"))

prices_unadjusted <- xts(prices_unadjusted[,-1], order.by = as.Date(prices_unadjusted$Date, format = "%d.%m.%Y"))

riskfree <- xts(riskfree[,-1], order.by = as.Date(riskfree$Date, format = "%d.%m.%Y"))

factor_returns <- xts(factor_returns[,-1], order.by = as.Date(factor_returns$Date, format = "%d.%m.%Y"))



#################
###  Ex 5.1  ###
#################

#1. Calculate Market Cap

for (i in 1:384){
  market_cap <- prices_unadjusted[,1:i]*shares[,1:i]
}  

#2.
#View(market_cap)
for (i in 1:361){
  medians <- as.data.frame(rowMedians(market_cap[1:i], na.rm = TRUE))
}


View(medians)
medians$Median_CAP <- medians[,1]
market_cap$Median_CAP <- medians$Median_CAP
View(market_cap)



#4.

lagges_book_values <- lag(book_values, k=6)

for (i in 1:384){
  book_to_market <- lagges_book_values[,1:i]/market_cap[,1:i]
}  
View(book_to_market)




#################
###  Ex 5.2  ###
#################





#################
###  Ex 5.3  ###
#################

