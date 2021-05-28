# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 4")
# setwd("C:/Users/p_lae/OneDrive - Universit?t Z?rich UZH/Dokumente/Universit?t Z?rich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 4")

############
# Packages #
############
# uncomment the following lines to install the packages
 # install.packages("xts")
 # install.packages("PerformanceAnalytics")
 # install.packages("psych")
 # install.packages("roll")
 # install.packages("data.table")
 # install.packages("zoo")
 # install.packages("portsort")
 # install.packages("TTR")
###########

# load libraries
library(xts)
library(PerformanceAnalytics)
library(psych)
library(roll)
library(data.table)
library(ggplot2)
library(matrixStats)
library(zoo)
library(portsort)
library(TTR)
library(dplyr)

###############
# Data import #
###############

prices_adjusted <- read.delim(file = 'A4_dataset_01.txt', header = TRUE, sep = '\t', dec = '.')
book_values <- read.delim(file = 'A4_dataset_02.txt', header = TRUE, sep = '\t', dec = '.')
shares <- read.delim(file = 'A4_dataset_03.txt', header = TRUE, sep = '\t', dec = '.')
prices_unadjusted <- read.delim(file = 'A4_dataset_04.txt', header = TRUE, sep = '\t', dec = '.')
riskfree <- read.delim(file = 'A4_dataset_05.txt', header = TRUE, sep = '\t', dec = '.')
factor_returns <- read.delim(file = 'A4_dataset_06.txt', header = TRUE, sep = '\t', dec = '.')

# create time-series files and get returns from prices
date_monthly <- prices_adjusted$Date
date_monthly <- as.data.frame(date_monthly)
prices_adjusted.ts <- xts(prices_adjusted[,-1], order.by = as.Date(prices_adjusted $Date, format = "%d.%m.%Y"))
returns <- Return.calculate(prices = prices_adjusted.ts, method = 'discrete')
book_values <- xts(book_values[,-1], order.by = as.Date(book_values$Date, format = "%d.%m.%Y"))

# remove book values below 0:
for (i in 1:nrow(book_values)) {
  for(j in 1:ncol(book_values)) {
    condition <- book_values[i,j] < 0
    if(!is.na(condition)) {
      if(condition) {
        book_values[i,j] <- NA
      }
    }
  }
}

shares <- xts(shares[,-1], order.by = as.Date(shares$Date, format = "%d.%m.%Y"))
prices_unadjusted <- xts(prices_unadjusted[,-1], order.by = as.Date(prices_unadjusted$Date, format = "%d.%m.%Y"))
factor_returns <- xts(factor_returns[,-1], order.by = as.Date(factor_returns$Date, format = "%d.%m.%Y"))

# turn riskfree into ts and divide by 100 because it is in percentages
riskfree <- xts(riskfree[,-1], order.by = as.Date(riskfree$Date,  format = "%d.%m.%Y"))
riskfree <- riskfree/100



#################
###  Ex 5.1  ###
#################

####### 1.

# Calculate Market Cap
for (i in 1:384){
  market_cap <- prices_unadjusted[,1:i]*shares[,1:i]
}  

####### 2.
# Calculate the monthly median value of the Market Capitalization
Median_CAP <- rowMedians(market_cap, na.rm=T)

# add value of 1 to values below median and NA otherwise
portfolio_S <- apply(market_cap, 2, function(S) ifelse(S <= Median_CAP, 1, NA))
# add value of 1 to values above median and NA otherwise
portfolio_B <- apply(market_cap, 2, function(B) ifelse(B > Median_CAP, 1, NA))


####### 3.

returns_S <- returns[2:nrow(returns),] * portfolio_S[1:nrow(returns)-1,]
returns_B <- returns[2:nrow(returns),] * portfolio_B[1:nrow(returns)-1,]

returns_S_short <- returns_S['1991-02-01/2019-12-01']
returns_B_short <- returns_B['1991-02-01/2019-12-01']

monthly_means_S <- rowMeans(returns_S_short, na.rm = T)
monthly_means_B <- rowMeans(returns_B_short, na.rm = T)

SMB_portfolio <- as.data.frame(monthly_means_S - monthly_means_B)
colnames(SMB_portfolio) <- c("PF_SMB")
annualized_mean_return <- (mean(SMB_portfolio$PF_SMB, na.rm = T)+1)^12-1
print(annualized_mean_return)

# Plot cumulative Returns 
Date <- date_monthly[15:nrow(date_monthly),, drop=F]
cumulative_returns <- cumprod(1+SMB_portfolio$PF_SMB) #doesn't work with 0
cum_returns <- cbind(Date, cumulative_returns)
cum_returns$Date <- as.Date(cum_returns$date_monthly, format = "%d.%m.%Y")
plot(cum_returns$Date, cum_returns$cumulative_returns, type = "l", lty = 1,  lwd = 3, col = "blue", ylab = "Cumulative Return", xlab = "Time")

# Calculate Sharpe Ratio
riskfreerate <- mean(riskfree['1991-02-01/2019-12-01']) #annualized risk-free
SD_portfolio <- sd(SMB_portfolio$PF_SMB)*sqrt(12)
SR_portfolio <- (annualized_mean_return-riskfreerate)/SD_portfolio
print(SR_portfolio)


####### 4. Book to market

lagged_book_values <- lag(book_values, k=6)
for (i in 1:384){
  book_to_market <- lagged_book_values[,1:i]/market_cap[,1:i]
}  


####### 7. Calculate median and assign to portfolios

Median_BTM <- rowMedians(book_to_market, na.rm = TRUE)

# add value of 1 to values below median and NA otherwise
portfolio_L <- apply(book_to_market, 2, function(L) ifelse(L <= Median_BTM, 1, NA))
# add value of 1 to values above median and NA otherwise
portfolio_H <- apply(book_to_market, 2, function(H) ifelse(H > Median_BTM, 1, NA))


####### 8.




####### 9. 

# get portfolio returns
returns_short <- returns['1991-02-01/2019-12-01']
returns_L <- returns_short * portfolio_L[15:nrow(portfolio_L)-1,]
returns_H <- returns_short * portfolio_H[15:nrow(portfolio_H)-1,]

monthly_means_L <- rowMeans(returns_L, na.rm = T)
monthly_means_H <- rowMeans(returns_H, na.rm = T)

HML_portfolio <- as.data.frame(monthly_means_H - monthly_means_L)
colnames(HML_portfolio) <- c("PF_HML")
annualized_mean_return_HML <- (mean(HML_portfolio$PF_HML, na.rm = T)+1)^12-1
print(annualized_mean_return_HML)

# Plot cumulative Returns 
Date <- date_monthly[15:nrow(date_monthly),, drop=F]
cumulative_returns_HML <- cumprod(1+HML_portfolio$PF_HML) #doesn't work with 0
cum_returns_HML <- cbind(Date, cumulative_returns_HML)
cum_returns_HML$Date <- as.Date(cum_returns_HML$date_monthly, format = "%d.%m.%Y")
plot(cum_returns_HML$Date, cum_returns_HML$cumulative_returns, type = "l", lty = 1,  lwd = 3, col = "blue", ylab = "Cumulative Return", xlab = "Time")

# Calculate Sharpe Ratio
SD_portfolio_HML <- sd(HML_portfolio$PF_HML)*sqrt(12)
SR_portfolio_HML <- (annualized_mean_return_HML-riskfreerate)/SD_portfolio_HML
print(SR_portfolio_HML)


####### 10. 
returns_short <- returns["1991-02-01/2019-12-01"]

returns_company <- round(colMeans(returns_short, na.rm = T), digits = 6)
print(returns_company)

####### 11.
momentum <- ROC(prices_adjusted.ts, n = 11, type =  "discrete", na.pad = F)

####### 12.
momentum_lagged <- lag(momentum, k=1)

medians_3 <- rowMedians(momentum_lagged, na.rm=T)

# Assign to PF U or D
portfolio_D <- apply(momentum_lagged, 2, function(D) ifelse(D <= medians_3, 1, NA))
portfolio_U <- apply(momentum_lagged, 2, function(U) ifelse(U > medians_3, 1, NA))

returns_short <- returns["19901101/20191201"]

returns_D <- returns_short[2:nrow(returns_short),] * portfolio_D[1:nrow(returns_short)-1,]
returns_U <- returns_short[2:nrow(returns_short),] * portfolio_U[1:nrow(returns_short)-1,]

returns_D_short <- returns_D['1991-02-01/2019-12-01']
returns_U_short <- returns_U['1991-02-01/2019-12-01']

monthly_means_D <- rowMeans(returns_D_short, na.rm = T)
monthly_means_U <- rowMeans(returns_U_short, na.rm = T)

MOM_portfolio <- as.data.frame(monthly_means_U - monthly_means_D)
colnames(MOM_portfolio) <- c("PF_MOM")
annualized_mean_return <- (mean(MOM_portfolio$PF_MOM, na.rm = T)+1)^12-1
print(annualized_mean_return)


# Plot cumulative Returns 
Date <- date_monthly[15:nrow(date_monthly),, drop=F]
cumulative_returns <- cumprod(1+MOM_portfolio$PF_MOM) #doesn't work with 0
cum_returns <- cbind(Date, cumulative_returns)
cum_returns$Date <- as.Date(cum_returns$date_monthly, format = "%d.%m.%Y")
plot(cum_returns$Date, cum_returns$cumulative_returns, type = "l", lty = 1,  lwd = 3, col = "blue", ylab = "Cumulative Return", xlab = "Time")

# Calculate Sharpe Ratio
riskfreerate <- mean(riskfree['1991-02-01/2019-12-01']) #annualized risk-free
SD_portfolio <- sd(MOM_portfolio$PF_MOM)*sqrt(12)
SR_portfolio <- (annualized_mean_return-riskfreerate)/SD_portfolio
print(SR_portfolio)


#################
###  Ex 5.2  ###
#################



####### 1.

# sort companies into 8 different groups (after adapting the format)
rn <- rownames(portfolio_S)
S <- xts(portfolio_S, order.by=as.Date(rn))
rn <- rownames(portfolio_B)
B <- xts(portfolio_B, order.by=as.Date(rn))

rn <- rownames(portfolio_H)
H <- xts(portfolio_H, order.by=as.Date(rn))
rn <- rownames(portfolio_L)
L <- xts(portfolio_L, order.by=as.Date(rn))

rn <- rownames(portfolio_U)
U <- xts(portfolio_U, order.by=as.Date(rn))
rn <- rownames(portfolio_D)
D <- xts(portfolio_D, order.by=as.Date(rn))

PF_SHU = S['19910101/20191201'] + H['19910101/20191201'] + U['19910101/20191201']
SHU <- apply(PF_SHU, 2, function(x) ifelse(x == 3, 1, NA))

PF_SLU = S['19910101/20191201'] + L['19910101/20191201'] + U['19910101/20191201']
SLU <- apply(PF_SLU, 2, function(x) ifelse(x == 3, 1, NA))

PF_SLD = S['19910101/20191201'] + L['19910101/20191201'] + D['19910101/20191201']
SLD <- apply(PF_SLD, 2, function(x) ifelse(x == 3, 1, NA))

PF_SHD = S['19910101/20191201'] + H['19910101/20191201'] + D['19910101/20191201']
SHD <- apply(PF_SHD, 2, function(x) ifelse(x == 3, 1, NA))

PF_BLD = B['19910101/20191201'] + L['19910101/20191201'] + D['19910101/20191201']
BLD <- apply(PF_BLD, 2, function(x) ifelse(x == 3, 1, NA))

PF_BHD = B['19910101/20191201'] + H['19910101/20191201'] + D['19910101/20191201']
BHD <- apply(PF_BHD, 2, function(x) ifelse(x == 3, 1, NA))

PF_BHU = B['19910101/20191201'] + H['19910101/20191201'] + U['19910101/20191201']
BHU <- apply(PF_BHU, 2, function(x) ifelse(x == 3, 1, NA))

PF_BLU = B['19910101/20191201'] + L['19910101/20191201'] + U['19910101/20191201']
BLU <- apply(PF_BLU, 2, function(x) ifelse(x == 3, 1, NA))


#lag to prevent look-ahead bias
#SHU_lead <- lead(SHU, n=1)
#SLU_lead <- lead(SLU, k=1)
#SLD_lead <- lead(SLD, k=1)
#SHD_lead <- lead(SHD, k=1)
#BLD_lead <- lead(BLD, k=1)
#BHD_lead <- lead(BHD, k=1)
#BLU_lead <- lead(BLU, k=1)
#BHU_lead <- lead(BHU, k=1)


####### 2.

#Mean size of the portfolios
Row_sum_SHU <- rowSums(SHU_lead[1:nrow(SHU_lead)-1,], na.rm = T)
Mean_Size_SHU <- mean(Row_sum_SHU)
print(Mean_Size_SHU)

Row_sum_SLU <- rowSums(SLU[1:nrow(SLU)-1,], na.rm = T)
Mean_Size_SLU <- mean(Row_sum_SLU)
print(Mean_Size_SLU)

Row_sum_SLD <- rowSums(SLD_lag, na.rm = T)
Mean_Size_SLD <- mean(Row_sum_SLD)
print(Mean_Size_SLD)

Row_sum_SHD <- rowSums(SHD_lag, na.rm = T)
Mean_Size_SHD <- mean(Row_sum_SHD)
print(Mean_Size_SHD)

Row_sum_BLD <- rowSums(BLD_lag, na.rm = T)
Mean_Size_BLD <- mean(Row_sum_BLD)
print(Mean_Size_BLD)

Row_sum_BHD <- rowSums(BHD_lag, na.rm = T)
Mean_Size_BHD <- mean(Row_sum_BHD)
print(Mean_Size_BHD)

Row_sum_BLU <- rowSums(BLU_lag, na.rm = T)
Mean_Size_BLU <- mean(Row_sum_BLU)
print(Mean_Size_BLU)

Row_sum_BHU <- rowSums(BHU_lag, na.rm = T)
Mean_Size_BHU <- mean(Row_sum_BHU)
print(Mean_Size_BHU)


####### 3.
# Create factor-mimicking-portfolios
returns_new <- returns["19910101/20191201"]

for (i in 1:384){
  returns_SHU <- returns_new[,1:i]*(as.numeric(SHU_lag[,1:i]))
} 

for (i in 1:384){
  returns_SLU <- returns_new[,1:i]*(as.numeric(SLU_lag[,1:i]))
} 

for (i in 1:384){
  returns_SLD <- returns_new[,1:i]*(as.numeric(SLD_lag[,1:i]))
} 

for (i in 1:384){
  returns_SHD <- returns_new[,1:i]*(as.numeric(SHD_lag[,1:i]))
} 

for (i in 1:384){
  returns_BLD <- returns_new[,1:i]*(as.numeric(BLD_lag[,1:i]))
} 

for (i in 1:384){
  returns_BHD <- returns_new[,1:i]*(as.numeric(BHD_lag[,1:i]))
} 

for (i in 1:384){
  returns_BLU <- returns_new[,1:i]*(as.numeric(BLU_lag[,1:i]))
} 

for (i in 1:384){
  returns_BHU <- returns_new[,1:i]*(as.numeric(BHU_lag[,1:i]))
} 



SMB <-  0.25*(returns_SHU-returns_BHU+returns_SHD-returns_BHD+returns_SLU-returns_BLU+returns_SLD-returns_BLD)

HML <- 0.25*(returns_SHU-returns_SLU+returns_SHD-returns_SLD+returns_BHU-returns_BLU+returns_BHD-returns_BLD)

MOM <- 0.25*(returns_SHU-returns_SHD+returns_SLU-returns_SLD+returns_BHU-returns_BHD+returns_BLU-returns_BLD)

####### 4.
#Mean Annualized returns

Annualized_return_SMB <- ((mean(rowMeans(SMB, na.rm = T), na.rm = T)+1)^(12)-1)
print(Annualized_return_SMB)

Annualized_return_HML <- ((mean(rowMeans(HML, na.rm = T), na.rm = T)+1)^(12)-1)
print(Annualized_return_HML)

Annualized_return_MOM <- ((mean(rowMeans(MOM, na.rm = T), na.rm = T)+1)^(12)-1)
print(Annualized_return_MOM)

####### 5. 




#################
###  Ex 5.3  ###
#################



#################
### COMMENTS ###
#################

# 1. Remove negative Book Values
# 2. Use Log on riskfree return ?

