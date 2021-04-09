# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 1")
# setwd("C:/Users/p_lae/OneDrive - Universität Zürich UZH/Dokumente/Universität Zürich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 2")

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

# turn interest_rates into ts and divide by 100 because it is in percentages and turn into monthly rates
interest_rates <- xts(interest_rates[,-1], order.by = as.Date(interest_rates$Date))
interest_rates_mon <- ((1+interest_rates/100)^(1/12)-1)


#################
###  Ex 5.1  ###
#################

####### 1.

# no code required


####### 2.

# no code required


####### 3.

# make regression for beta and other parameters for the four companies(first calculate SMI excess return and the same for each stock)
SMI_excess <- SMI_TotRet_mon$SMI.Total.Return['2016-03-31/2021-02-26'] - interest_rates_mon$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['2016-03-31/2021-02-26']

adecco_excess <- returns$Adecco['2016-03-31/2021-02-26'] - interest_rates_mon$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_adecco <- lm(adecco_excess ~SMI_excess)
result_adecco <- summary(fit_adecco)

row_adecco <- c(result_adecco$coefficients[2,1],
                result_adecco$coefficients[1,1],
                result_adecco$r.squared,
                sigma(fit_adecco),
                result_adecco$coefficients[2,2],
                result_adecco$coefficients[1,2])

cs_excess <- returns$Credit_Suisse_Group['2016-03-31/2021-02-26'] - interest_rates_mon$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_cs <- lm(cs_excess ~SMI_excess)
result_cs<- summary(fit_cs)

row_cs <- c(result_cs$coefficients[2,1],
                result_cs$coefficients[1,1],
                result_cs$r.squared,
                sigma(fit_cs),
                result_cs$coefficients[2,2],
                result_cs$coefficients[1,2])

lafarge_excess <- returns$LafargeHolcim['2016-03-31/2021-02-26'] - interest_rates_mon$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['2016-03-31/2021-02-26']
fit_lafarge <- lm(lafarge_excess ~SMI_excess)
result_lafarge<- summary(fit_lafarge)

row_lafarge <- c(result_lafarge$coefficients[2,1],
                result_lafarge$coefficients[1,1],
                result_lafarge$r.squared,
                sigma(fit_lafarge),
                result_lafarge$coefficients[2,2],
                result_lafarge$coefficients[1,2])

swisscom_excess <- returns$Swisscom['2016-03-31/2021-02-26'] - interest_rates_mon$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['2016-03-31/2021-02-26']
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


####### 7.

# make beta time series file
date <- as.Date(betas[,1])
betas.ts <- xts(x = betas[,-1], order.by = date)

# make table with previously calculated betas and add the new time period
comparison_betas <- data.table(Adecco = c(result_adecco$coefficients[2,1], mean(betas.ts$Adecco['2007-07-02/2009-03-31'])),
                       Credit_Suisse = c(result_cs$coefficients[2,1], mean(betas.ts$Credit_Suisse_Group['2007-07-02/2009-03-31'])),
                       LafargeHolcim = c(result_lafarge$coefficients[2,1], mean(betas.ts$LafargeHolcim['2007-07-02/2009-03-31'])),
                       Swisscom = c(result_swisscom$coefficients[2,1], mean(betas.ts$Swisscom['2007-07-02/2009-03-31'])))
# add row with absolute difference
comparison_betas <- rbind(comparison_betas,abs(comparison_betas[1,]-comparison_betas[2,]))



#################
###  Ex 5.2  ###
#################

###### 1.

# Credit Suisse
cs_rolling <- returns$Credit_Suisse_Group['1994-02-28/2021-02-26']
market_rolling_cs <- SMI_TotRet_mon$SMI.Total.Return['1994-02-28/2021-02-26']
riskfree <- interest_rates_mon$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['1994-02-28/2021-02-26']

# present beta
rolling_beta_cs <- rollapplyr(data = cs_rolling, width = 60, FUN = CAPM.beta, Rb = market_rolling_cs, Rf = riskfree, by = 1, align = "right", by.column = TRUE)

# calculate past beta  
rolling_beta_lag_cs <- lag.xts(rolling_beta_cs)
rollbeta_merged_cs <- merge(rolling_beta_cs, rolling_beta_lag_cs)

# create linear model with past and present beta to determine intercept and slope
model_cs <- lm(rollbeta_merged_cs$Credit_Suisse_Group ~ rollbeta_merged_cs$Credit_Suisse_Group.1)
summary_cs <- summary(model_cs)

# calculate predicted beta of last present beta entry from CS
predicted_betas <- model_cs$coefficients[1] + model_cs$coefficients[2] * last(rollbeta_merged_cs$Credit_Suisse_Group)
sprintf("predicted beta: %f", predicted_betas)
sprintf("alpha hat: %f", model_cs$coefficients[1])
sprintf("beta hat: %f", model_cs$coefficients[2])

# Adecco
ad_rolling <- returns$Adecco['1988-07-29/2021-02-26']
market_rolling_ad <- SMI_TotRet_mon$SMI.Total.Return['1988-07-29/2021-02-26']
riskfree_ad <- interest_rates_mon$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['1988-07-29/2021-02-26']
rolling_beta_ad <- rollapplyr(data = ad_rolling, width = 60, FUN = CAPM.beta, Rb = market_rolling_ad, Rf = riskfree_ad, by = 1, align = "right", by.column = TRUE)
rolling_beta_lag_ad <- lag.xts(rolling_beta_ad)
rollbeta_merged_ad <- merge(rolling_beta_ad, rolling_beta_lag_ad)
model_ad <- lm(rollbeta_merged_ad$Adecco ~ rollbeta_merged_ad$Adecco.1)
summary_ad <- summary(model_ad)
predicted_betas_ad <- model_ad$coefficients[1] + model_ad$coefficients[2] * last(rollbeta_merged_ad$Adecco)
sprintf("predicted beta: %f", predicted_betas_ad)
sprintf("alpha hat: %f", model_ad$coefficients[1])
sprintf("beta hat: %f", model_ad$coefficients[2])


# Lafarge Holcim
lh_rolling <- returns$LafargeHolcim['2001-06-29/2021-02-26']
market_rolling_lh <- SMI_TotRet_mon$SMI.Total.Return['2001-06-29/2021-02-26']
riskfree_lh <- interest_rates_mon$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['2001-06-29/2021-02-26']
rolling_beta_lh <- rollapplyr(data = lh_rolling, width = 60, FUN = CAPM.beta, Rb = market_rolling_lh, Rf = riskfree_lh, by = 1, align = "right", by.column = TRUE)
rolling_beta_lag_lh <- lag.xts(rolling_beta_lh)
rollbeta_merged_lh <- merge(rolling_beta_lh, rolling_beta_lag_lh)
model_lh <- lm(rollbeta_merged_lh$LafargeHolcim ~ rollbeta_merged_lh$LafargeHolcim.1)
summary_lh <- summary(model_lh)
predicted_betas_lh <- model_lh$coefficients[1] + model_lh$coefficients[2] * last(rollbeta_merged_lh$LafargeHolcim)
sprintf("predicted beta: %f", predicted_betas_lh)
sprintf("alpha hat: %f", model_lh$coefficients[1])
sprintf("beta hat: %f", model_lh$coefficients[2])


# Swisscom
sc_rolling <- returns$Swisscom['1998-11-30/2021-02-26']
market_rolling_sc <- SMI_TotRet_mon$SMI.Total.Return['1998-11-30/2021-02-26']
riskfree_sc <- interest_rates_mon$SWISS.CONFEDERATION.BOND.5.YEAR...RED..YIELD['1998-11-30/2021-02-26']
rolling_beta_sc <- rollapplyr(data = sc_rolling, width = 60, FUN = CAPM.beta, Rb = market_rolling_sc, Rf = riskfree_sc, by = 1, align = "right", by.column = TRUE)
rolling_beta_lag_sc <- lag.xts(rolling_beta_sc)
rollbeta_merged_sc <- merge(rolling_beta_sc, rolling_beta_lag_sc)
model_sc <- lm(rollbeta_merged_sc$Swisscom ~ rollbeta_merged_sc$Swisscom.1)
summary_sc <- summary(model_sc)
predicted_betas_sc <- model_sc$coefficients[1] + model_sc$coefficients[2] * last(rollbeta_merged_sc$Swisscom)
sprintf("predicted beta: %f", predicted_betas_sc)
sprintf("alpha hat: %f", model_sc$coefficients[1])
sprintf("beta hat: %f", model_sc$coefficients[2])



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




