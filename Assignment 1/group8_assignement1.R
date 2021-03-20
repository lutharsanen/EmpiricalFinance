# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 1")
# setwd("C:/Users/p_lae/OneDrive - Universität Zürich UZH/Dokumente/Universität Zürich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 1")

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


#iii)

MC_monthly <- xts(x = market_values[,-1], order.by = date)
# Create a vector of the total market cap of the index for a given month
totMC_monthly <- rowSums(MC_monthly, na.rm = TRUE)

#Compute weights
VW_weights <- MC_monthly / totMC_monthly

# a)

max_novartis <-VW_weights[which.max(VW_weights$Novartis_I),]
print(max_novartis$Novartis_I)

max_nestle <-VW_weights[which.max(VW_weights$Nestle_I),]
print(max_nestle$Nestle_I)

max_roche <-VW_weights[which.max(VW_weights$Roche_Holding),]
print(max_roche$Roche_Holding)

max_ubs <-VW_weights[which.max(VW_weights$UBS_I),]
print(max_ubs$UBS_I)

# b)

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

print(prod(1 + portfolio_VW['1988-06-30/2000-12-29']))
print(prod(1 + portfolio_EW['1988-06-30/2000-12-29']))

print(prod(1 + portfolio_VW['2000-12-29/2021-02-26']))
print(prod(1 + portfolio_EW['2000-12-29/2021-02-26']))

print(prod(1 + portfolio_VW['1998-07-31/2012-12-31']))
print(prod(1 + portfolio_EW['1998-07-31/2012-12-31']))

print(prod(1 + portfolio_VW['2007-05-31/2017-12-29']))
print(prod(1 + portfolio_EW['2007-05-31/2017-12-29']))

print(prod(1 + portfolio_VW['2011-08-31/2020-01-31']))
print(prod(1 + portfolio_EW['2011-08-31/2020-01-31']))

#f)

maxDrawdown(portfolio_VW['1988-06-30/2000-12-29'])
maxDrawdown(portfolio_EW['1988-06-30/2000-12-29'])

maxDrawdown(portfolio_VW['2000-12-29/2020-01-31'])
maxDrawdown(portfolio_EW['2000-12-29/2020-01-31'])

maxDrawdown(portfolio_VW['2020-02-28/2021-02-26'])
maxDrawdown(portfolio_EW['2020-02-28/2021-02-26'])

#5

#a)

base_data <- data.frame(portfolio_EW)
adjuster <- tail(cpi[2],-1) 
adjusted_data <- base_data / (adjuster/100)

mean_return_adjusted <- mean(adjusted_data[["portfolio_EW"]])
pf_annualized_adjusted <- (((1+mean_return_adjusted)^12)-1)

print(pf_annualized_adjusted)

#b)

base_data_VW <- (portfolio_VW)
cpi.ts <- xts(x = cpi[,-1], order.by = date)
adjusted_data_VW <- base_data_VW / (cpi.ts$CPI_as_of_88.06/100)

#####################################

cpi.ts <- xts(x = cpi[,-1], order.by = date)

#######################################

print(prod(1 + adjusted_data_VW['1988-07-29/2021-02-26']))

cum_ret_portfolio_adj <- (cumprod(1 + adjusted_data_VW['1988-07-29/2021-02-26']))
cum_ret_portfolio_non_adj <- (cumprod(1 + portfolio_VW['1988-07-29/2021-02-26']))


print(prod(1 + portfolio_VW['1988-07-29/2021-02-26']))

plot(x=date[0:391], y=cum_ret_portfolio_adj[2:392], type = "l", lty = 1, lwd =3, col = "black", 
     cex.axis = 1.5, cex.lab = 1.5, ylab = "Cumulative Returns", xlab = "Date", main= "Equal vs Value-weighting")
lines(date[0:391], cum_ret_portfolio_non_adj[2:392], type = "l", lty = 1, lwd = 3, col = "gray")
legend("topleft", legend = c("EW Portfolio", "VW Portfolio"), lty = 1, lwd = 3, col = c("gray","black"))

#################
###  Ex 5.2  ###
#################

#1. Calculate Sharp Ratio and SD of the first stock in Dataset 2 using 1-year Swiss Gov. Bond as risk free rate, see Dataset 3

#Mean Return
mean_return_stock <- mean(rowMeans(returns.ts[,1], na.rm = TRUE), na.rm = TRUE)
return_annualized_stock <- Return.annualized(mean_return_stock, scale = 12, geometric = TRUE)
return_annualized_stock

#risk free mean return
annual_rf <- interest_rates_ts[,2]
riskfree <- mean (annual_rf/100, na.rm=TRUE)
riskfree

#rolling SD
#subset last 12 observation periods
SD_stock <- mean(roll_sd(rowMeans(returns.ts[,1],na.rm=TRUE),12,na_restore=TRUE),na.rm=TRUE)*12^0.5
SD_stock

SR_stock <- (return_annualized_stock-riskfree)/SD_stock
SR_stock





###########################################################################################################
#Portfolio with 25 stocks
#Mean Returns
mean_return_portfolio25 <- mean(rowMeans(returns.ts[,1:25], na.rm = TRUE), na.rm = TRUE)
return_annualized_portfolio25 <- Return.annualized(mean_return_portfolio25, scale = 12, geometric = TRUE)
return_annualized_portfolio25

#risk free mean return
annual_rf <- interest_rates_ts[,2]
riskfree <- mean (monthly_rf/100, na.rm=TRUE)
riskfree

#rolling SD
#subset last 12 observation periods

SD_portfolio25 <- mean(roll_sd(rowMeans(returns.ts[,1:25],na.rm=TRUE),12,na_restore=TRUE),na.rm=TRUE)*12^0.5
SD_portfolio25

SR_portfolio25 <- (return_annualized_portfolio25-riskfree)/SD_portfolio25
SR_portfolio25


###########################################################################################################
#make a loop


annual_rf <- interest_rates_ts[,2]
riskfree <- mean (monthly_rf/100, na.rm=TRUE)
riskfree

for (i in seq(1,25,2)){
  mean_return_portfolio <- mean(rowMeans(returns.ts[,1:i], na.rm = TRUE), na.rm = TRUE)
  print(paste("Mean", i, ":", mean_return_portfolio))
  return_annualized_portfolio <- Return.annualized(mean_return_portfolio, scale = 12, geometric = TRUE)
  print(paste("Mean annualized", i, ":", return_annualized_portfolio))
  SD_portfolio <- mean(roll_sd(rowMeans(returns.ts[,1:i],na.rm=TRUE),12,na_restore=TRUE),na.rm=TRUE)*12^0.5
  print(paste("SD",i,":",SD_portfolio))
  SR_portfolio <- (return_annualized_portfolio-riskfree)/SD_portfolio
  print(paste("SR",i,":",SR_portfolio))
}


for (i in seq(30,60,5)){
  mean_return_portfolio <- mean(rowMeans(returns.ts[,1:i], na.rm = TRUE), na.rm = TRUE)
  print(paste("Mean", i, ":", mean_return_portfolio))
  return_annualized_portfolio <- Return.annualized(mean_return_portfolio, scale = 12, geometric = TRUE)
  print(paste("Mean annualized", i, ":", return_annualized_portfolio))
  SD_portfolio <- mean(roll_sd(rowMeans(returns.ts[,1:i],na.rm=TRUE),12,na_restore=TRUE),na.rm=TRUE)*12^0.5
  print(paste("SD",i,":",SD_portfolio))
  SR_portfolio <- (return_annualized_portfolio-riskfree)/SD_portfolio
  print(paste("SR",i,":",SR_portfolio))
}


#################
###  Ex 5.3  ###
#################

# i) no code required

# ii)

# calculate annualized standard deviation for equal and value weighted returns
pf_annualized_std_EW <- sd(portfolio_EW)*sqrt(12)
pf_annualized_std_VW <- sd(portfolio_VW)*sqrt(12)

# calculate 12, 24, 36 and 48 months return (+1)
pf_EW_12 <- rollapplyr(1+portfolio_EW, na.rm=T, 12, prod, fill=NA)
pf_EW_24 <- rollapplyr(1+portfolio_EW, na.rm=T, 24, prod, fill=NA)
pf_EW_36 <- rollapplyr(1+portfolio_EW, na.rm=T, 36, prod, fill=NA)
pf_EW_48 <- rollapplyr(1+portfolio_EW, na.rm=T, 48, prod, fill=NA)

pf_VW_12 <- rollapplyr(1+portfolio_VW, na.rm=T, 12, prod, fill=NA)
pf_VW_24 <- rollapplyr(1+portfolio_VW, na.rm=T, 24, prod, fill=NA)
pf_VW_36 <- rollapplyr(1+portfolio_VW, na.rm=T, 36, prod, fill=NA)
pf_VW_48 <- rollapplyr(1+portfolio_VW, na.rm=T, 48, prod, fill=NA)

# calculate variance ratios
VR_EW_1 <- (var(portfolio_EW)/var(pf_EW_12-1,na.rm=T))*(12/1)
VR_EW_24 <- (var(pf_EW_24-1,na.rm=T)/var(pf_EW_12-1,na.rm=T))*(12/24)
VR_EW_36 <- (var(pf_EW_36-1,na.rm=T)/var(pf_EW_12-1,na.rm=T))*(12/36)
VR_EW_48 <- (var(pf_EW_48-1,na.rm=T)/var(pf_EW_12-1,na.rm=T))*(12/48)

VR_VW_1 <- (var(portfolio_VW)/var(pf_VW_12-1,na.rm=T))*(12/1)
VR_VW_24 <- (var(pf_VW_24-1,na.rm=T)/var(pf_VW_12-1,na.rm=T))*(12/24)
VR_VW_36 <- (var(pf_VW_36-1,na.rm=T)/var(pf_VW_12-1,na.rm=T))*(12/36)
VR_VW_48 <- (var(pf_VW_48-1,na.rm=T)/var(pf_VW_12-1,na.rm=T))*(12/48)

# iii) no code required

# iv)

# calculate log returns and repeat ii)
log_portfolio_EW <- log(portfolio_EW+1)
log_portfolio_VW <- log(portfolio_VW+1)

# calculate annualized standard deviation for equal and value weighted returns
log_pf_annualized_std_EW <- sd(log_portfolio_EW)*sqrt(12)
log_pf_annualized_std_VW <- sd(log_portfolio_VW)*sqrt(12)

# calculate 12, 24, 36 and 48 months return (+1)
log_pf_EW_12 <- rollapplyr(1+log_portfolio_EW, na.rm=T, 12, prod, fill=NA)
log_pf_EW_24 <- rollapplyr(1+log_portfolio_EW, na.rm=T, 24, prod, fill=NA)
log_pf_EW_36 <- rollapplyr(1+log_portfolio_EW, na.rm=T, 36, prod, fill=NA)
log_pf_EW_48 <- rollapplyr(1+log_portfolio_EW, na.rm=T, 48, prod, fill=NA)

log_pf_VW_12 <- rollapplyr(1+log_portfolio_VW, na.rm=T, 12, prod, fill=NA)
log_pf_VW_24 <- rollapplyr(1+log_portfolio_VW, na.rm=T, 24, prod, fill=NA)
log_pf_VW_36 <- rollapplyr(1+log_portfolio_VW, na.rm=T, 36, prod, fill=NA)
log_pf_VW_48 <- rollapplyr(1+log_portfolio_VW, na.rm=T, 48, prod, fill=NA)

# calculate variance ratios
log_VR_EW_1 <- (var(log_portfolio_EW)/var(log_pf_EW_12-1,na.rm=T))*(12/1)
log_VR_EW_24 <- (var(log_pf_EW_24-1,na.rm=T)/var(log_pf_EW_12-1,na.rm=T))*(12/24)
log_VR_EW_36 <- (var(log_pf_EW_36-1,na.rm=T)/var(log_pf_EW_12-1,na.rm=T))*(12/36)
log_VR_EW_48 <- (var(log_pf_EW_48-1,na.rm=T)/var(log_pf_EW_12-1,na.rm=T))*(12/48)

log_VR_VW_1 <- (var(log_portfolio_VW)/var(log_pf_VW_12-1,na.rm=T))*(12/1)
log_VR_VW_24 <- (var(log_pf_VW_24-1,na.rm=T)/var(log_pf_VW_12-1,na.rm=T))*(12/24)
log_VR_VW_36 <- (var(log_pf_VW_36-1,na.rm=T)/var(log_pf_VW_12-1,na.rm=T))*(12/36)
log_VR_VW_48 <- (var(log_pf_VW_48-1,na.rm=T)/var(log_pf_VW_12-1,na.rm=T))*(12/48)