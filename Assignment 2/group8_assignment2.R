# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 1")
# setwd("C:/Users/p_lae/OneDrive - Universit?t Z?rich UZH/Dokumente/Universit?t Z?rich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 1")

############
# Packages #
############
# uncomment the three following lines to install the packages
# install.packages("xts")
# install.packages("PerformanceAnalytics")
# install.packages("psych")
# install.packages("roll")
###########

# load libraries
library(xts)
library(PerformanceAnalytics)
library(psych)
library(roll)



###############
# Data import #
###############
prices_monthly <- read.delim(file = 'A2_dataset_01.txt', header = TRUE, sep = '\t', dec = '.')
interest_rates <- read.delim(file = 'A2_dataset_02.txt', header = TRUE, sep = '\t', dec = '.')
SMI_monthly <- read.delim(file = 'A2_dataset_03.txt', header = TRUE, sep = '\t', dec = '.')
prices_daily <- read.delim(file = 'A2_dataset_04.txt', header = TRUE, sep = '\t', dec = '.')
betas <- read.delim(file = 'A2_dataset_05.txt', header = TRUE, sep = '\t', dec = '.')

# create monthly returns for stocks and SMI
date <- as.Date(prices_monthly[,1])
prices.ts <- xts(x = prices_monthly[,-1], order.by = date)
returns <- Return.calculate(prices = prices.ts, method = 'discrete')

SMI_monthly <- xts(SMI_monthly[,-1], order.by = as.Date(SMI_monthly$Date))
SMI_TotRet_mon <- Return.calculate(SMI_monthly)

# turn interest_rates into ts and divide by 100 because it is in percentages
interest_rates <- xts(interest_rates[,-1], order.by = as.Date(interest_rates$Date))
interest_rates <- interest_rates/100


#################
###  Ex 5.1  ###
#################

####### 1.

# no code required


####### 2.

# no code required


####### 3.

# make regression for beta --> only up to August 2020 (because of unavailable data for Adecco)
fit_adecco <- lm(returns$Adecco['2016-03-31/2020-8-31'] 
                - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2020-8-31']  
                ~ SMI_TotRet_mon$SMI.Total.Return['2016-03-31/2020-8-31']
                - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2020-8-31'])
result_adecco<- summary(fit_adecco)
result_adecco



#################
###  Ex 5.2  ###
#################

###### 1.


###### 2.



#################
###  Ex 5.3  ###
#################

###### 1. 

###### 2.

