# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 1")
# setwd("C:/Users/p_lae/OneDrive - Universit?t Z?rich UZH/Dokumente/Universit?t Z?rich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 2")

############
# Packages #
############
# uncomment the three following lines to install the packages
 install.packages("xts")
 install.packages("PerformanceAnalytics")
 install.packages("psych")
 install.packages("roll")
 install.packages("data.table")
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

# make regression for beta and other parameters for the four companies(first calculate SMI excess return and the same for each stock)
SMI_excess <- SMI_TotRet_mon$SMI.Total.Return['2016-03-31/2021-02-26'] - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2021-02-26']

adecco_excess <- returns$Adecco['2016-03-31/2021-02-26'] - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_adecco <- lm(adecco_excess ~SMI_excess)
result_adecco <- summary(fit_adecco)

row_adecco <- c(result_adecco$coefficients[2,1],
                result_adecco$coefficients[1,1],
                result_adecco$r.squared,
                sigma(fit_adecco),
                result_adecco$coefficients[2,2],
                result_adecco$coefficients[1,2])

cs_excess <- returns$Credit_Suisse_Group['2016-03-31/2021-02-26'] - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_cs <- lm(cs_excess ~SMI_excess)
result_cs<- summary(fit_cs)

row_cs <- c(result_cs$coefficients[2,1],
                result_cs$coefficients[1,1],
                result_cs$r.squared,
                sigma(fit_cs),
                result_cs$coefficients[2,2],
                result_cs$coefficients[1,2])

lafarge_excess <- returns$LafargeHolcim['2016-03-31/2021-02-26'] - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_lafarge <- lm(lafarge_excess ~SMI_excess)
result_lafarge<- summary(fit_lafarge)

row_lafarge <- c(result_lafarge$coefficients[2,1],
                result_lafarge$coefficients[1,1],
                result_lafarge$r.squared,
                sigma(fit_lafarge),
                result_lafarge$coefficients[2,2],
                result_lafarge$coefficients[1,2])

swisscom_excess <- returns$Swisscom['2016-03-31/2021-02-26'] - interest_rates$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_swisscom <- lm(swisscom_excess ~SMI_excess)
result_swisscom<- summary(fit_swisscom)

row_swisscom <- c(result_swisscom$coefficients[2,1],
                result_swisscom$coefficients[1,1],
                result_swisscom$r.squared,
                sigma(fit_swisscom),
                result_swisscom$coefficients[2,2],
                result_swisscom$coefficients[1,2])

# put results into a table
beta_table <- rbind(row_adecco, row_cs, row_lafarge, row_swisscom)
beta_table <- round(beta_table,4)
rownames(beta_table) <- c("Adecco", "Credit Suisse", "LafargeHolcim", "Swisscom")
colnames(beta_table) <- c("Beta", "Alpha", "R2", "Res. SD", "SE Beta", "SE Alpha")


####### 4.

# no code needed

####### 5.

# make table with Beta t-values
t_values <- data.table(Adecco = result_adecco$coefficients[2,3],
                       Credit_Suisse = result_cs$coefficients[2,3],
                       LafargeHolcim = result_lafarge$coefficients[2,3],
                       Swisscom= result_swisscom$coefficients[2,3])


####### 6.
AF_betas <- data.table(Adecco = (result_adecco$coefficients[2,1]*2/3+1/3),
                       Credit_Suisse = (result_cs$coefficients[2,1]*2/3+1/3),
                       LafargeHolcim = (result_lafarge$coefficients[2,1]*2/3+1/3),
                       Swisscom= (result_swisscom$coefficients[2,1]*2/3+1/3))


#################
###  Ex 5.2  ###
#################

###### 1.

#use the function roll::roll_lm of "roll" package to compute rolling window betas 
#width=60 means the rolling window acounts for 60 periods.
#intercept=True means alpha is allowed.
apply.rolling(cs_excess[,1,drop=FALSE], width=60)


###### 2.

#no code required

#################
###  Ex 5.3  ###
#################

prices_daily <- A2_dataset_04
#View(prices_daily)
#betas <- A2_dataset_05
#View(betas)


date <- as.Date(prices_daily[,1])
prices_daily.ts <- xts(x = prices_daily[,-1], order.by = date)

date_2 <- as.Date(betas[,1])
betas.ts <- xts(x = betas[,-1], order.by = date_2)


###### 1. 
# Calculate Returns

returns_daily <- Return.calculate(prices = prices_daily.ts, method = 'discrete')
# View(returns_daily)

returns_daily.ts <- as.matrix(returns_daily['1994-04-30/2017-12-29'])
# View(returns_daily.ts)

daily_betas.ts <- as.matrix(betas.ts['1994-04-29/2017-12-28'])
View(daily_betas.ts)


# create empty vectors for loop
Returns_P1_daily <- as.matrix(rep(NA,dim(daily_betas.ts)[1]))
Returns_P2_daily <- as.matrix(rep(NA,dim(daily_betas.ts)[1]))
Returns_P3_daily <- as.matrix(rep(NA,dim(daily_betas.ts)[1]))
Returns_P4_daily <- as.matrix(rep(NA,dim(daily_betas.ts)[1]))
Returns_P5_daily <- as.matrix(rep(NA,dim(daily_betas.ts)[1]))


for (j in 1:dim(daily_betas.ts)[1])
{
  
  Subset_daily <- daily_betas.ts[j,] #for each day so every row j
  
  
  P1_P_daily <- subset(Subset_daily,subset = Subset_daily < quantile(Subset_daily , c(0.2),na.rm=TRUE))
  P2_P_daily <- subset(Subset_daily,subset = Subset_daily < quantile(Subset_daily , c(0.4),na.rm=TRUE)& Subset_daily >= quantile(Subset_daily , c(0.2),na.rm=TRUE))
  P3_P_daily <- subset(Subset_daily,subset = Subset_daily < quantile(Subset_daily , c(0.6),na.rm=TRUE)& Subset_daily >= quantile(Subset_daily , c(0.4),na.rm=TRUE))
  P4_P_daily <- subset(Subset_daily,subset = Subset_daily < quantile(Subset_daily , c(0.8),na.rm=TRUE)& Subset_daily >= quantile(Subset_daily , c(0.6),na.rm=TRUE))
  P5_P_daily <- subset(Subset_daily,subset = Subset_daily >= quantile(Subset_daily, c(0.8),na.rm=TRUE))
  
  # calculate mean returns
  
  Returns_P1_daily[j] <- mean(returns_daily.ts[j,names(P1_P_daily)],na.rm=TRUE)
  Returns_P2_daily[j] <- mean(returns_daily.ts[j,names(P2_P_daily)],na.rm=TRUE)
  Returns_P3_daily[j] <- mean(returns_daily.ts[j,names(P3_P_daily)],na.rm=TRUE)
  Returns_P4_daily[j] <- mean(returns_daily.ts[j,names(P4_P_daily)],na.rm=TRUE)
  Returns_P5_daily[j] <- mean(returns_daily.ts[j,names(P5_P_daily)],na.rm=TRUE)
}

# Mean returns

mean_return1 <- (mean(Returns_P1_daily,na.rm=TRUE))
mean_return2 <- (mean(Returns_P2_daily,na.rm=TRUE))
mean_return3 <- (mean(Returns_P3_daily,na.rm=TRUE))
mean_return4 <- (mean(Returns_P4_daily,na.rm=TRUE))
mean_return5 <- (mean(Returns_P5_daily,na.rm=TRUE))

round(mean_return1, digits=6)
round(mean_return2, digits=6)
round(mean_return3, digits=6)
round(mean_return4, digits=6)
round(mean_return5, digits=6)


# Mean standard deviations of the portfolios

sd_return1 <- (sd(Returns_P1_daily,na.rm=TRUE))
sd_return2 <- (sd(Returns_P2_daily,na.rm=TRUE))
sd_return3 <- (sd(Returns_P3_daily,na.rm=TRUE))
sd_return4 <- (sd(Returns_P4_daily,na.rm=TRUE))
sd_return5 <- (sd(Returns_P5_daily,na.rm=TRUE))

round(sd_return1, digits=6)
round(sd_return2, digits=6)
round(sd_return3, digits=6)
round(sd_return4, digits=6)
round(sd_return5, digits=6)


###### 2.


date_daily2 <- date_2[2:6176]
View(date_daily2)

# plot results

cumulative_returns_p1_daily <- cumprod(1+Returns_P1_daily)
cumulative_returns_p2_daily <- cumprod(1+Returns_P2_daily)
cumulative_returns_p3_daily <- cumprod(1+Returns_P3_daily)
cumulative_returns_p4_daily <- cumprod(1+Returns_P4_daily)
cumulative_returns_p5_daily <- cumprod(1+Returns_P5_daily)

plot(x=date_daily2, y=cumulative_returns_p1_daily, ylim=c(0,18),type= "l", lty = 1, lwd = 3, col = "turquoise", cex.axis = 1, cex.lab = 1, ylab = "Cumulative Return", xlab = "Time")
lines(date_daily2, cumulative_returns_p2_daily, lty = 1, lwd = 3, col = "blue4")
lines(date_daily2, cumulative_returns_p3_daily, lty = 1, lwd = 2, col = "cadetblue")
lines(date_daily2, cumulative_returns_p4_daily, lty = 1, lwd = 2, col = "darkorchid")
lines(date_daily2, cumulative_returns_p5_daily, lty = 1, lwd = 2, col = "green4")

legend("topleft", c("Portfolio 1", "Portfolio 2", "Portfolio 3", "Portfolio 4", "Portfolio 5"), 
       lty = c(1,1,1,1,1), lwd = 3, bty = "n",cex = 1.2, col = c("turquoise", "blue4", "cadetblue","darkorchid","green4"))




