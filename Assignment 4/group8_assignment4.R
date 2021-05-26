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
install.packages("portsort")
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
date_monthly$Date <- prices_adjusted$Date
as.data.frame(date_monthly)

prices_adjusted <- xts(prices_adjusted[,-1], order.by = as.Date(prices_adjusted $Date, format = "%d.%m.%Y"))
returns <- Return.calculate(prices = prices_adjusted, method = 'log')

book_values <- xts(book_values[,-1], order.by = as.Date(book_values$Date, format = "%d.%m.%Y"))
#####remove book values below 0:

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
riskfree <- xts(riskfree[,-1], order.by = as.Date(riskfree$Date, format = "%d.%m.%Y"))
factor_returns <- xts(factor_returns[,-1], order.by = as.Date(factor_returns$Date, format = "%d.%m.%Y"))



#################
###  Ex 5.1  ###
#################

####### 1.

# Calculate Market Cap
for (i in 1:384){
  market_cap <- prices_unadjusted[,1:i]*shares[,1:i]
}  

####### 2.
#View(market_cap)
for (i in 1:361){
  medians <- as.data.frame(rowMedians(market_cap[1:i], na.rm = TRUE))
}

market_cap$Median_CAP <- medians[,1]

View(market_cap)

portfolio_B <- market_cap

for (i in 1:nrow(market_cap)) {
  for(j in 1:ncol(market_cap)) {
    condition <- market_cap[i,ncol(market_cap)] < market_cap[i,j]
    if(!is.na(condition)) {
      if(condition) {
        portfolio_B[i,j] <- 1
      }
      else {
        portfolio_B[i,j] <- 0
      } 
    }
  }
}




#We incoorporate all values equal to the median in the portfolio small.

portfolio_S <- market_cap

for (i in 1:nrow(market_cap)) {
  for(j in 1:ncol(market_cap)) {
    condition <- market_cap[i,ncol(market_cap)] >= market_cap[i,j]
    if(!is.na(condition)) {
      if(condition) {
        portfolio_S[i,j] <- 1
      }
      else {
        portfolio_S[i,j] <- 0
      } 
    }
  }
}


####### 3.
lag_portfolio_S <- lag(portfolio_S, k=1)
lag_portfolio_B <- lag(portfolio_B, k=1)

#View(lag_portfolio_B)

#Calculate mean returns for S
for (i in 1:384){
  returns_S <- returns[,1:i]*(as.numeric(lag_portfolio_S[,1:i]))
} 
#View(returns_S)

#Calculate mean returns for B
for (i in 1:384){
  returns_B <- returns[,1:i]*(as.numeric(lag_portfolio_B[,1:i]))
} 
#View(returns_B)

returns_SMB <- returns_S - returns_B
#View(returns_SMB)


# annualized mean return
mean_returns <- rowMeans(returns_SMB, na.rm = TRUE)

annualized_mean_return <- (((mean(mean_returns, na.rm = TRUE))+1)^(1/12)-1)*100
annualized_mean_return


# Plot cumulative Returns 
mean_returns <- as.data.frame(mean_returns)
mean_returns <- mean_returns[2:nrow(mean_returns),, drop=F]
#View(mean_returns) #360 entries

Date <- date_monthly[2:nrow(date_monthly),, drop=F]

cumulative_returns <- cumprod(1+mean_returns)

cum_returns <- cbind(Date, cumulative_returns)
cum_returns$Date <- as.Date(cum_returns$Date, format = "%d.%m.%Y")
#View(cum_returns)

plot(cum_returns$Date, cum_returns$mean_returns, type = "l", lty = 1,  lwd = 3, col = "blue", ylab = "Cumulative Return", xlab = "Time")

# Calculate Sharpe Ratio

riskfreerate <- mean(riskfree) #annualized rsikfree

mean_returns$annualized_returns <- ((mean_returns$mean_returns+1)^(1/12)-1)*100

SD_portfolio <- sd(mean_returns$annualized_returns) #is that correct ??

SR_portfolio <- (annualized_mean_return-riskfreerate)/SD_portfolio
print(SR_portfolio)



####### 4. Book to market

lagged_book_values <- lag(book_values, k=6)

for (i in 1:384){
  book_to_market <- lagged_book_values[,1:i]/market_cap[,1:i]
}  
#View(book_to_market)


####### 7. Calculate median and assign to portfolios

for (i in 1:nrow(book_to_market)){
  medians_2 <- as.data.frame(rowMedians(book_to_market[1:i], na.rm = TRUE))
}
#View(medians_2)

book_to_market$medians2 <- medians_2[,1]

View(book_to_market)

portfolio_H <- book_to_market

for (i in 1:nrow(book_to_market)) {
  for(j in 1:ncol(book_to_market)) {
    condition <- book_to_market[i,ncol(book_to_market)] < book_to_market[i,j]
    if(!is.na(condition)) {
      if(condition) {
        portfolio_H[i,j] <- 1
      }
      else {
        portfolio_H[i,j] <- 0
      } 
    }
  }
}



portfolio_L <- book_to_market

for (i in 1:nrow(book_to_market)) {
  for(j in 1:ncol(book_to_market)) {
    condition <- book_to_market[i,ncol(book_to_market)] >= book_to_market[i,j]
    if(!is.na(condition)) {
      if(condition) {
        portfolio_L[i,j] <- 1
      }
      else {
        portfolio_L[i,j] <- 0
      } 
    }
  }
}


####### 8.

# Jan 1998: UBS in Portfolio High; Nestle Roche and Novartis are in portfolio Low
# Jan 2008: All big four are in the Low portfolio



####### 9. 
lag_portfolio_L <- lag(portfolio_L, k=1)
lag_portfolio_H <- lag(portfolio_H, k=1)

#View(lag_portfolio_B)

#Calculate mean returns for S
for (i in 1:384){
  returns_L <- returns[,1:i]*(as.numeric(lag_portfolio_L[,1:i]))
} 
View(returns_L)

#Calculate mean returns for B
for (i in 1:384){
  returns_H <- returns[,1:i]*(as.numeric(lag_portfolio_H[,1:i]))
} 
View(returns_B)

returns_LH <- returns_L - returns_H
View(returns_LH)


# annualized mean return
mean_returns_LH <- rowMeans(returns_LH, na.rm = TRUE)

annualized_mean_return_LH <- (((mean(mean_returns_LH, na.rm = TRUE))+1)^(1/12)-1)*100
annualized_mean_return_LH


# Plot cumulative Returns 
mean_returns_LH <- as.data.frame(mean_returns_LH)
mean_returns_LH <- mean_returns_LH[8:nrow(mean_returns_LH),, drop=F]

View(mean_returns_LH) #354 entries

Date_LH <- date_monthly[8:nrow(date_monthly),, drop=F]

cumulative_returns_LH <- cumprod(1+mean_returns_LH)

cum_returns_LH <- cbind(Date_LH, cumulative_returns_LH)
cum_returns_LH$Date <- as.Date(cum_returns_LH$Date , format = "%d.%m.%Y")
#View(cum_returns)

plot(cum_returns_LH$Date, cum_returns_LH$mean_returns_LH, type = "l", lty = 1,  lwd = 3, col = "blue", ylab = "Cumulative Return", xlab = "Time")

# Calculate Sharpe Ratio

#riskfreerate <- mean(riskfree) #annualized rsikfree

mean_returns_LH$annualized_returns_LH <- ((mean_returns_LH$mean_returns_LH+1)^(1/12)-1)*100

SD_portfolio <- sd(mean_returns_LH$annualized_returns_LH) #is that correct ??

SR_portfolio <- (annualized_mean_return_LH-riskfreerate)/SD_portfolio
print(SR_portfolio)


####### 10. 
returns_company <- round(colMeans(returns, na.rm = T), digits = 6)
View(returns_company)


####### 11.
#install.packages("TTR")
library(TTR)
momentum <- ROC(prices_adjusted, n = 11, type =  "discrete", na.pad = F)
View(momentum)


####### 12.


for (i in 1:nrow(momentum)){
  medians_3 <- as.data.frame(rowMedians(momentum[1:i], na.rm = TRUE))
}

View(medians_3)

momentum$medians3 <- medians_3[,1]

momentum_lagged <- lag(momentum, k=1)
  
  
portfolio_D <- momentum_lagged #go short on Loser Comapnies D

for (i in 1:nrow(momentum_lagged)) {
  for(j in 1:ncol(momentum_lagged)) {
    condition <- momentum_lagged[i,ncol(momentum_lagged)] >= momentum_lagged[i,j] # loser stocks median is bigger or equal to value
    if(!is.na(condition)) {
      if(condition) {
        portfolio_D[i,j] <- 1
      }
      else {
        portfolio_D[i,j] <- 0
      } 
    }
  }
}



portfolio_U <- momentum_lagged #go long on Winner Comapnies U

for (i in 1:nrow(momentum_lagged)) {
  for(j in 1:ncol(momentum_lagged)) {
    condition <- momentum_lagged[i,ncol(momentum_lagged)] < momentum_lagged[i,j] #Winner portfolio median is smaller than values
    if(!is.na(condition)) {
      if(condition) {
        portfolio_U[i,j] <- 1
      }
      else {
        portfolio_U[i,j] <- 0
      } 
    }
  }
}

#lag again to prevent look-ahead bias
lag_portfolio_U <- lag(portfolio_U, k=1)
lag_portfolio_D <- lag(portfolio_D, k=1)
View(lag_portfolio_U)

returns_short <- returns["19901101/20191201"]

#Calculate mean returns for U
for (i in 1:384){
  returns_U <- returns_short[,1:i]*(as.numeric(lag_portfolio_U[,1:i]))
} 
View(returns_U)

#Calculate mean returns for D
for (i in 1:384){
  returns_D <- returns_short[,1:i]*(as.numeric(lag_portfolio_D[,1:i]))
} 
View(returns_D)

####### 13.

returns_UD <- returns_U - returns_D
View(returns_UD)


# annualized mean return
mean_returns_UD <- rowMeans(returns_UD, na.rm = TRUE)

annualized_mean_return_UD <- (((mean(mean_returns_UD, na.rm = TRUE))+1)^(1/12)-1)*100
annualized_mean_return_UD

# Plot cumulative Returns 
mean_returns_UD <- as.data.frame(mean_returns_UD)
View(mean_returns_UD)
mean_returns_UD_short <- mean_returns_UD[3:nrow(mean_returns_UD),, drop=F]
View(mean_returns_UD_short) #348 entries

View(date_monthly)
Date_UD <- date_monthly[14:nrow(date_monthly),, drop=F] #from 01.01.91
View(Date_UD)#348 entries

cumulative_returns_UD <- cumprod(1+mean_returns_UD_short)

cum_returns_UD <- cbind(Date_UD, cumulative_returns_UD)
cum_returns_UD$Date <- as.Date(cum_returns_UD$date_monthly , format = "%d.%m.%Y")
View(cum_returns_UD)

plot(cum_returns_UD$Date, cum_returns_UD$mean_returns_UD, type = "l", lty = 1,  lwd = 3, col = "blue", ylab = "Cumulative Return", xlab = "Time")

# Calculate Sharpe Ratio

#riskfreerate <- mean(riskfree) #annualized rsikfree

mean_returns_UD_short$annualized_returns_UD <- ((mean_returns_UD_short$mean_returns_UD+1)^(1/12)-1)*100

SD_portfolio <- sd(mean_returns_UD_short$annualized_returns_UD) #is that correct ??

SR_portfolio <- (annualized_mean_return_UD-riskfreerate)/SD_portfolio
print(SR_portfolio) 



#################
###  Ex 5.2  ###
#################



####### 1.
portfolio_S_new <- portfolio_S["19910101/20191201"]
portfolio_B_new <- portfolio_B["19910101/20191201"]
portfolio_H_new <- portfolio_H["19910101/20191201"]
portfolio_L_new <- portfolio_L["19910101/20191201"]
portfolio_U_new <- portfolio_U["19910101/20191201"]
portfolio_D_new <- portfolio_D["19910101/20191201"]


SHU_sums <- portfolio_S_new + portfolio_H_new + portfolio_U_new
SHU <- SHU_sums
for (i in 1:nrow(SHU_sums)) {
  for(j in 1:384) {
    condition <-  SHU_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        SHU[i,j] <- 1
      }
      else {
        SHU[i,j] <- 0
      } 
    }
  }
}
#View(SHU)

SLU_sums <- portfolio_S_new + portfolio_L_new + portfolio_U_new
SLU <- SLU_sums
for (i in 1:nrow(SLU_sums)) {
  for(j in 1:384) {
    condition <-  SLU_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        SLU[i,j] <- 1
      }
      else {
        SLU[i,j] <- 0
      } 
    }
  }
}

SLD_sums <- portfolio_S_new + portfolio_L_new + portfolio_D_new
SLD <- SLD_sums
for (i in 1:nrow(SLD_sums)) {
  for(j in 1:384) {
    condition <-  SLD_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        SLD[i,j] <- 1
      }
      else {
        SLD[i,j] <- 0
      } 
    }
  }
}

SHD_sums <- portfolio_S_new + portfolio_H_new + portfolio_D_new
SHD <- SHD_sums
for (i in 1:nrow(SHD_sums)) {
  for(j in 1:384) {
    condition <-  SHD_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        SHD[i,j] <- 1
      }
      else {
        SHD[i,j] <- 0
      } 
    }
  }
}

BLD_sums <- portfolio_B_new + portfolio_L_new + portfolio_D_new
BLD <- BLD_sums
for (i in 1:nrow(BLD_sums)) {
  for(j in 1:384) {
    condition <-  BLD_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        BLD[i,j] <- 1
      }
      else {
        BLD[i,j] <- 0
      } 
    }
  }
}


BHD_sums <- portfolio_B_new + portfolio_H_new + portfolio_D_new
BHD <- BHD_sums
for (i in 1:nrow(BHD_sums)) {
  for(j in 1:384) {
    condition <-  BHD_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        BHD[i,j] <- 1
      }
      else {
        BHD[i,j] <- 0
      } 
    }
  }
}

BLU_sums <- portfolio_B_new + portfolio_L_new + portfolio_U_new
BLU <- BLU_sums
for (i in 1:nrow(BLU_sums)) {
  for(j in 1:384) {
    condition <-  BLU_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        BLU[i,j] <- 1
      }
      else {
        BLU[i,j] <- 0
      } 
    }
  }
}


BHU_sums <- portfolio_B_new + portfolio_H_new + portfolio_U_new
BHU <- BHU_sums
for (i in 1:nrow(BHU_sums)) {
  for(j in 1:384) {
    condition <-  BHU_sums[i,j] == 3
    if(!is.na(condition)) {
      if(condition) {
        BHU[i,j] <- 1
      }
      else {
        BHU[i,j] <- 0
      } 
    }
  }
}



#lag to prevent look-ahead bias
SHU_lag <- lag(SHU, k=1)
SLU_lag <- lag(SLU, k=1)
SLD_lag <- lag(SLD, k=1)
SHD_lag <- lag(SHD, k=1)
BLD_lag <- lag(BLD, k=1)
BHD_lag <- lag(BHD, k=1)
BLU_lag <- lag(BLU, k=1)
BHU_lag <- lag(BHU, k=1)

#View(BHD_lag)

####### 2.

#Mean size of the portfolios
Row_sum_SHU <- rowSums(SHU_lag, na.rm = T)
Mean_Size_SHU <- mean(Row_sum_SHU/nrow(SHU_lag))
print(Mean_Size_SHU)

Row_sum_SLU <- rowSums(SLU_lag, na.rm = T)
Mean_Size_SLU <- mean(Row_sum_SLU/nrow(SLU_lag))
print(Mean_Size_SLU)

Row_sum_SLD <- rowSums(SLD_lag, na.rm = T)
Mean_Size_SLD <- mean(Row_sum_SLD/nrow(SLD_lag))
print(Mean_Size_SLD)

Row_sum_SHD <- rowSums(SHD_lag, na.rm = T)
Mean_Size_SHD <- mean(Row_sum_SHD/nrow(SHD_lag))
print(Mean_Size_SHD)

Row_sum_BLD <- rowSums(BLD_lag, na.rm = T)
Mean_Size_BLD <- mean(Row_sum_BLD/nrow(BLD_lag))
print(Mean_Size_BLD)

Row_sum_BHD <- rowSums(BHD_lag, na.rm = T)
Mean_Size_BHD <- mean(Row_sum_BHD/nrow(BHD_lag))
print(Mean_Size_BHD)

Row_sum_BLU <- rowSums(BLU_lag, na.rm = T)
Mean_Size_BLU <- mean(Row_sum_BLU/nrow(BLU_lag))
print(Mean_Size_BLU)

Row_sum_BHU <- rowSums(BHU_lag, na.rm = T)
Mean_Size_BHU <- mean(Row_sum_BHU/nrow(BHU_lag))
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
View(SMB)


HML <- 0.25*(returns_SHU-returns_SLU+returns_SHD-returns_SLD+returns_BHU-returns_BLU+returns_BHD-returns_BLD)
View(HML)


MOM <- 0.25*(returns_SHU-returns_SHD+returns_SLU-returns_SLD+returns_BHU-returns_BHD+returns_BLU-returns_BLD)
View(MOM)


####### 4.
#Mean Annualized returns

Annualized_return_SMB <- (mean(rowMeans(SMB, na.rm = T), na.rm = T)+1)^(1/12)-1
print(Annualized_return_SMB)

Annualized_return_HML <- (mean(rowMeans(HML, na.rm = T), na.rm = T)+1)^(1/12)-1
print(Annualized_return_HML)

Annualized_return_MOM <- (mean(rowMeans(MOM, na.rm = T), na.rm = T)+1)^(1/12)-1
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







