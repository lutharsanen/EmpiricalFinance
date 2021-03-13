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
###  Ex 5.1a  ###
#################

# i)

date <- as.Date(prices[,1])
prices.ts <- xts(x = prices[,-1], order.by = date)
returns_results <- Return.calculate(prices = prices.ts, method = 'discrete')

# ii)
portfolio_EW_returns <- rowMeans(returns_results, na.rm=TRUE)
portfolio_EW_returnsts <- xts(portfolio_EW_returns, order.by = date)

portfolio_EW <- portfolio_EW_returnsts[-1,]

mean_geometric_return <- mean(portfolio_EW)
pf_annualized <- (((1+mean_geometric_return)^12)-1)

mean(pf_annualized)

#iii)

3
# a)

max_novartis <-(max(market_values$Novartis_I, na.rm = TRUE))
