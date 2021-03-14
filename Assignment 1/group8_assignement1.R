# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 1")

############
# Packages #
############
# uncomment the three following lines to install the packages
# install.packages("xts")
# install.packages("PerformanceAnalytics")
# install.packages("psych")
###########

# load libraries
library(xts)
library(PerformanceAnalytics)
library(psych)



###############
# Data import #
###############
prices <- read.delim(file = 'A1_dataset_01.txt', header = TRUE, sep = '\t', dec = '.')
returns <- read.delim(file = 'A1_dataset_02.txt', header = TRUE, sep = '\t', dec = '.')
interest_rates <- read.delim(file = 'A1_dataset_03.txt', header = TRUE, sep = '\t', dec = '.')
market_values <- read.delim(file = 'A1_dataset_04.txt', header = TRUE, sep = '\t', dec = '.')
cpi <- read.delim(file = 'A1_dataset_05.txt', header = TRUE, sep = '\t', dec = '.')

View(prices[,1])
View(prices[1,])
View(prices[1,1])

#################
###  Ex 5.1  ###
#################

# i)

date <- as.Date(prices[,1])
prices.ts <- xts(x = prices[,-1], order.by = date)
returns_results <- Return.calculate(prices = prices.ts, method = 'discrete')

# ii)
portfolio_EW_returns <- rowMeans(returns_results, na.rm=TRUE)
portfolio_EW_returnsts <- xts(portfolio_EW_returns, order.by = date)

portfolio_EW <- portfolio_EW_returnsts[-1,]

mean_return_EW <- mean(portfolio_EW)
pf_annualized_EW <- (((1+mean_return_EW)^12)-1)

mean(pf_annualized_EW)

#iii)

# a)

max_novartis <-market_values[which.max(market_values$Novartis_I),]
print(max_novartis[c("Date","Novartis_I")])

max_nestle <-market_values[which.max(market_values$Nestle_I),]
print(max_nestle[c("Date","Nestle_I")])

max_roche <-market_values[which.max(market_values$Roche_Holding),]
print(max_roche[c("Date","Roche_Holding")])

max_ubs <-market_values[which.max(market_values$UBS_I),]
print(max_ubs[c("Date","UBS_I")])

# b)

MC_monthly <- xts(x = market_values[,-1], order.by = date)
# Create a vector of the total market cap of the index for a given month
totMC_monthly <- rowSums(MC_monthly, na.rm = TRUE)

#Compute weights
VW_weights <- MC_monthly / totMC_monthly

#Lag marketcap
lag_VW_weights <- lag.xts(VW_weights, 1)

#Now we multiply the weights by the returns
vw_returns <- lag_VW_weights * returns_results

#Now we sum this across all stock to get the portfolio return
portfolio_VW_returns <- rowSums(vw_returns, na.rm = TRUE)
portfolio_VW_returnsts <- xts(portfolio_VW_returns, order.by = date)

#Drop the first row since that should be missing 
portfolio_VW <- portfolio_VW_returnsts[-1,]

mean_return_VW <- mean(portfolio_VW)
pf_annualized_VW <- (((1+mean_return_VW)^12)-1)

#4

#a)

print(prod(1 + portfolio_VW_returnsts['1988-06-30/2000-12-29']))
print(prod(1 + portfolio_EW_returnsts['1988-06-30/2000-12-29']))

print(prod(1 + portfolio_VW_returnsts['2000-12-29/2021-02-26']))
print(prod(1 + portfolio_EW_returnsts['2000-12-29/2021-02-26']))

print(prod(1 + portfolio_VW_returnsts['1998-07-31/2012-12-31']))
print(prod(1 + portfolio_EW_returnsts['1998-07-31/2012-12-31']))

print(prod(1 + portfolio_VW_returnsts['2007-05-31/2017-12-29']))
print(prod(1 + portfolio_EW_returnsts['2007-05-31/2017-12-29']))

print(prod(1 + portfolio_VW_returnsts['2011-08-31/2020-01-31']))
print(prod(1 + portfolio_EW_returnsts['2011-08-31/2020-01-31']))

#f)

maxDrawdown(portfolio_VW_returnsts['1988-06-30/2000-12-29'])
maxDrawdown(portfolio_EW_returnsts['1988-06-30/2000-12-29'])

maxDrawdown(portfolio_VW_returnsts['2000-12-29/2020-01-31'])
maxDrawdown(portfolio_EW_returnsts['2000-12-29/2020-01-31'])

maxDrawdown(portfolio_VW_returnsts['2020-02-28/2020-02-26'])
maxDrawdown(portfolio_EW_returnsts['2020-02-28/2020-02-26'])

#5

#################
###  Ex 5.2  ###
#################

# i)


#################
###  Ex 5.3  ###
#################

# i) no code required

# ii)


