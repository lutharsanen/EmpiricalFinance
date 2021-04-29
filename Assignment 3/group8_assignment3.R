# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 3")
# setwd("C:/Users/p_lae/OneDrive - Universität Zürich UZH/Dokumente/Universität Zürich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 3")

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



###############
# Data import #
###############
prices <- read.delim(file = 'A3_dataset_01.txt', header = TRUE, sep = '\t', dec = '.')
SMI_monthly <- read.delim(file = 'A3_dataset_02.txt', header = TRUE, sep = '\t', dec = '.')
interest_rates <- read.delim(file = 'A2_dataset_03.txt', header = TRUE, sep = '\t', dec = '.')

# # create monthly returns for stocks and SMI
# date <- as.Date(prices_monthly[,1])
# prices.ts <- xts(x = prices_monthly[,-1], order.by = date)
# returns <- Return.calculate(prices = prices.ts, method = 'discrete')
# 
# SMI_monthly <- xts(SMI_monthly[,-1], order.by = as.Date(SMI_monthly$Date))
# SMI_TotRet_mon <- Return.calculate(SMI_monthly)
# 
# # turn interest_rates into ts and divide by 100 because it is in percentages and turn into monthly rates
# interest_rates <- xts(interest_rates[,-1], order.by = as.Date(interest_rates$Date))
# interest_rates_mon <- ((1+interest_rates/100)^(1/12)-1)


#################
###  Ex 5.1  ###
#################

####### 1.




####### 2.



####### 3.



####### 4.


####### 5.



####### 6.




#################
###  Ex 5.2  ###
#################

###### 1.




###### 2.


###### 3.


###### 4.


###### 5.


###### 6.



#################
###  Ex 5.3  ###
#################

#no code required
