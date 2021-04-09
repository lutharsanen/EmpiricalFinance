###############################################################################

## Empirical Finance
## Assignment 2
## Authors: Giuseppe Scafiti, Nicola Meier, David Bacher

###############################################################################

## Preparing Data

# Clean environment
rm(list = ls())

# Install packages
# install.packages("stargazer")
# install.packages("xts")
# install.packages("zoo")
# install.packages("PerformanceAnalytics")
# install.packages("ggplot2")

# Load Packages
library("xts")
library("zoo")
library("PerformanceAnalytics")
library("ggplot2")
library("stargazer")

# Import Dataset 1
MonthlyStocks <- read.delim("A2_dataset_01.txt", sep = "\t", header = TRUE, na = "NA")
MonthlyStocks$Date <- as.Date(MonthlyStocks$Date)
MonthlyStocks <- xts(MonthlyStocks[,-1], order.by = MonthlyStocks$Date)

# Import Dataset 2
SwissBond <- read.delim("A2_dataset_02.txt",sep = "\t", header = TRUE, na = "NA")
SwissBond$Date <- as.Date(SwissBond$Date)
SwissBond <- xts(SwissBond[,-1], order.by = SwissBond$Date)
SwissBond_M <- ((1+SwissBond/100)^(1/12)-1) #to have them monthly

# Import Dataset 3
SMI <- read.delim("A2_dataset_03.txt",sep = "\t", header = TRUE, na = "NA")
SMI$Date <- as.Date(SMI$Date)
SMI_TR <- xts(SMI[, "SMI.Total.Return"], order.by = SMI$Date)

# Import Dataset 4
DailyStocks <- read.delim("A2_dataset_04.txt",sep = "\t", header = TRUE, na = "NA")
DailyStocks$Date <- as.Date(DailyStocks$Date)
DailyStocks <- xts(DailyStocks[,-1], order.by = DailyStocks$Date) 

# Import Dataset 5
DailyBetas <- read.delim("A2_dataset_05.txt",sep = "\t", header = TRUE, na = "NA")
DailyBetas$Date <- as.Date(DailyBetas$Date)
DailyBetas <- xts(DailyBetas[,-1], order.by = DailyBetas$Date)

###############################################################################################################
###############################################################################################################

### Section 5.1 - Estimating Betas

## 5.1.3 to 5.1.6 - Calculate the asked values in the table, the Adj. Beta and the t-value for each stock

# Now we compute the excess stocks return as well as the market excess return

Allmonthlyreturn<-Return.calculate(MonthlyStocks)
Marketreturn <- Return.calculate(SMI_TR)
MarketExcessReturn<-Marketreturn-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD


# Adecco
AdeccoExreturn<-Allmonthlyreturn$Adecco-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD
LM_Adecco <- lm(AdeccoExreturn["2016-03-31/2021-02-26"] ~
                  MarketExcessReturn["2016-03-31/2021-02-26"])
Beta_Adecco <- LM_Adecco$coef[2]
AdjBeta_Adecco <- (2/3) * LM_Adecco$coef[2] + 1/3

residuals_Adecco <- residuals(LM_Adecco)
stdev_res_Adecco <- sd(residuals_Adecco)
stdev_res_Adecco_annualized <- stdev_res_Adecco*sqrt(12)


# Credit Suisse

CreditSuisseExreturn<-Allmonthlyreturn$Credit_Suisse_Group-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD
LM_CreditSuisse <- lm(CreditSuisseExreturn["2016-03-31/2021-02-26"] ~
                        MarketExcessReturn["2016-03-31/2021-02-26"])
Beta_CreditSuisse <- LM_CreditSuisse$coef[2]
AdjBeta_CreditSuisse <- (2/3) * LM_CreditSuisse$coef[2] + 1/3

residuals_CreditSuisse <- residuals(LM_CreditSuisse)
stdev_res_CreditSuisse <- sd(residuals_CreditSuisse)
stdev_res_CreditSuisse_annualized <- stdev_res_CreditSuisse*sqrt(12)




# LafargeHolcim

LafExcessreturn<-Allmonthlyreturn$LafargeHolcim-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD
LM_LafargeHolcim <-lm(LafExcessreturn["2016-03-31/2021-02-26"] ~
                        MarketExcessReturn["2016-03-31/2021-02-26"])
Beta_LafargeHolcim <- LM_LafargeHolcim$coef[2]
AdjBeta_LafargeHolcim <- (2/3) * LM_LafargeHolcim$coef[2] + 1/3

residuals_LafargeHolcim <- residuals(LM_LafargeHolcim)
stdev_res_LafargeHolcim <- sd(residuals_LafargeHolcim)
stdev_res_LafargeHolcim_annualized <- stdev_res_LafargeHolcim*sqrt(12)


# Swisscom

SwisscomExcessreturn<-Allmonthlyreturn$Swisscom-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD
LM_Swisscom <- lm(SwisscomExcessreturn["2016-03-31/2021-02-26"] ~
                    MarketExcessReturn["2016-03-31/2021-02-26"])
Beta_Swisscom <- LM_Swisscom$coef[2]
AdjBeta_Swisscom <- (2/3) * LM_Swisscom$coef[2] + 1/3

residuals_Swisscom <- residuals(LM_Swisscom)
stdev_res_Swisscom <- sd(residuals_Swisscom)
stdev_res_Swisscom_annualized <- stdev_res_Swisscom*sqrt(12)


# Overview Estimated_Betas and Adj. Betas

Estimated_Betas<-matrix(0,ncol = 4,nrow = 2)
colnames(Estimated_Betas)<-c("Adecco","Credit Suisse","LafargeHolcim","Swisscom")
row.names(Estimated_Betas)<-c("Estimated Beta","Adjusted Beta")
Estimated_Betas[1,1]<-Beta_Adecco
Estimated_Betas[2,1]<-AdjBeta_Adecco
Estimated_Betas[1,2]<-Beta_CreditSuisse
Estimated_Betas[2,2]<-AdjBeta_CreditSuisse
Estimated_Betas[1,3]<-Beta_LafargeHolcim
Estimated_Betas[2,3]<-AdjBeta_LafargeHolcim
Estimated_Betas[1,4]<-Beta_Swisscom
Estimated_Betas[2,4]<-AdjBeta_Swisscom
print(Estimated_Betas)


# Creating Table inclusive the adj. beta and t-values of each beta for task 5.1.5 and 5.1.6

Table_plus<-matrix(0,ncol = 9,nrow = 4, dimnames = list(c("Adecco","Credit Suisse","LafargeHolcim","Swisscom"),
                                                   c("Beta","Alpha","R2","Res. Std. Dev.","Std. Err. Beta","Std. Err. Alpha", "Adj. Beta","t-value", "p-value")))
Table_plus[, "Beta"] <- c(LM_Adecco$coefficients[2], 
                    LM_CreditSuisse$coefficients[2], 
                    LM_LafargeHolcim$coefficients[2], 
                    LM_Swisscom$coefficients[2])
Table_plus[, "Alpha"] <- c(LM_Adecco$coefficients[1], 
                    LM_CreditSuisse$coefficients[1], 
                    LM_LafargeHolcim$coefficients[1], 
                    LM_Swisscom$coefficients[1])
Table_plus[, "R2"] <- c(summary(LM_Adecco)$r.squared, 
                    summary(LM_CreditSuisse)$r.squared, 
                    summary(LM_LafargeHolcim)$r.squared, 
                    summary(LM_Swisscom)$r.squared)
Table_plus[,"Res. Std. Dev."]<- c(stdev_res_Adecco_annualized, 
                   stdev_res_CreditSuisse_annualized, 
                   stdev_res_LafargeHolcim_annualized, 
                   stdev_res_Swisscom_annualized)
Table_plus[,"Std. Err. Beta"]<- c(summary(LM_Adecco)$coeff[2,2], 
                   summary(LM_CreditSuisse)$coeff[2,2], 
                   summary(LM_LafargeHolcim)$coeff[2,2], 
                   summary(LM_Swisscom)$coeff[2,2])
Table_plus[,"Std. Err. Alpha"]<- c(summary(LM_Adecco)$coeff[1,2], 
                   summary(LM_CreditSuisse)$coeff[1,2], 
                   summary(LM_LafargeHolcim)$coeff[1,2], 
                   summary(LM_Swisscom)$coeff[1,2])
Table_plus[, "Adj. Beta"]<- c(AdjBeta_Adecco, 
                   AdjBeta_CreditSuisse, 
                   AdjBeta_LafargeHolcim, 
                   AdjBeta_Swisscom)
Table_plus[, "t-value"]<- c(summary(LM_Adecco)$coeff[2,3], 
                   summary(LM_CreditSuisse)$coeff[2,3], 
                   summary(LM_LafargeHolcim)$coeff[2,3], 
                   summary(LM_Swisscom)$coeff[2,3])
Table_plus[, "p-value"]<- c(summary(LM_Adecco)$coeff[2,4], 
                            summary(LM_CreditSuisse)$coeff[2,4], 
                            summary(LM_LafargeHolcim)$coeff[2,4], 
                            summary(LM_Swisscom)$coeff[2,4])
print(Table_plus)

#################################################

##  5.1.7 - Computing the average beta for the four companies

# Creating table with estimated beta, average beta and difference in percentage
Beta_table <- matrix(NA, ncol = 3, nrow = 4, dimnames = list(c("Adecco","Credit Suisse","LafargeHolcim","Swisscom"),
                                                             c("Estimated Beta", "Average Beta", "Std. Dev. Avg. Beta")))
Beta_table[,"Estimated Beta"] <- Table_plus[, "Beta"]

# Adecco
Adecco_Average_Beta <- mean(DailyBetas$Adecco["2007-07-02/2009-03-31"])
Beta_table[1,"Average Beta"] <- Adecco_Average_Beta 

# Credit Suisse
CS_Average_Beta <- mean(DailyBetas$Credit_Suisse_Group["2007-07-02/2009-03-31"])
Beta_table[2, "Average Beta"] <- CS_Average_Beta 

# LafargeHolcim
LafargeHolcim_Average_Beta <- mean(DailyBetas$LafargeHolcim["2007-07-02/2009-03-31"])
Beta_table[3, "Average Beta"] <- LafargeHolcim_Average_Beta

# Swisscom
Swisscom_Average_Beta <- mean(DailyBetas$Swisscom["2007-07-02/2009-03-31"])
Beta_table[4,"Average Beta"] <- Swisscom_Average_Beta 

# Computing changes in sd

Beta_table[1,"Std. Dev. Avg. Beta"]<-sd(DailyBetas$Adecco["2007-07-02/2009-03-31"])
Beta_table[2,"Std. Dev. Avg. Beta"]<-sd(DailyBetas$Credit_Suisse_Group["2007-07-02/2009-03-31"])
Beta_table[3,"Std. Dev. Avg. Beta"]<-sd(DailyBetas$LafargeHolcim["2007-07-02/2009-03-31"])
Beta_table[4,"Std. Dev. Avg. Beta"]<-sd(DailyBetas$Swisscom["2007-07-02/2009-03-31"])


print(Beta_table)



###############################################################################################################
###############################################################################################################

### Section 5.2 - Predicting Betas

## 5.2.1 - Predicting Betas  

# Estimating betas and calculate predicting models

#Credit Suisse
CS_rolling_return <- Allmonthlyreturn$Credit_Suisse_Group['1994-02-28/2021-02-26']
Market_rolling_CS <- Marketreturn['1994-02-28/2021-02-26']
rf <- SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['1994-02-28/2021-02-26']


RollbetaCS <- rollapply(data = CS_rolling_return, width = 60, FUN = CAPM.beta, Rb = Market_rolling_CS, Rf = rf, by = 1, align = "right", by.column = TRUE)

RollbetaCS$Beta <- RollbetaCS$Credit_Suisse_Group
RollbetaCS$Credit_Suisse_Group <- NULL
Rollbeta_Lag1 <- lag.xts(RollbetaCS, k = +1)
Rollbeta_Real_CS <- merge(RollbetaCS, Rollbeta_Lag1)
Rollbeta_Real_CS$past_beta <- Rollbeta_Real_CS$Beta.1
Rollbeta_Real_CS$Beta.1 <- NULL

Model_CS <- lm(Rollbeta_Real_CS$Beta ~ Rollbeta_Real_CS$past_beta)
summary(Model_CS)
stargazer(Model_CS,type = "text")


# Adecco
Adecco<-Allmonthlyreturn$Adecco['1993-05-31/2021-02-26']
Market_Adecco<-Marketreturn['1993-05-31/2021-02-26']
rf_1<-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['1993-05-31/2021-02-26']

RollbetaAdecco<-rollapply(data=Adecco, width = 60,FUN=CAPM.beta, Rb=Market_Adecco, Rf=rf_1,by=1, align = "right", by.column = TRUE)

RollbetaAdecco$Beta<- RollbetaAdecco$Adecco
RollbetaAdecco$Adecco <- NULL
Rollbeta_Lag1_Adecco <- lag.xts(RollbetaAdecco, k = +1)
Rollbeta_Real_Adecco <-merge(RollbetaAdecco,Rollbeta_Lag1_Adecco)
Rollbeta_Real_Adecco$past_beta <- Rollbeta_Real_Adecco$Beta.1
Rollbeta_Real_Adecco$Beta.1 <- NULL

Model_Adecco <- lm(Rollbeta_Real_Adecco$Beta~Rollbeta_Real_Adecco$past_beta)
stargazer(Model_Adecco,type = "text")


# LafargeHolcim
Lafarge<-Allmonthlyreturn$LafargeHolcim['2001-06-29/2021-02-26']
Market_Lafarge<-Marketreturn['2001-06-29/2021-02-26']
rf_2<-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['2001-06-29/2021-02-26']

RollbetaLafarge<-rollapply(data=Lafarge, width = 60,FUN=CAPM.beta, Rb=Market_Lafarge, Rf=rf_2,by=1, align = "right", by.column = TRUE)

RollbetaLafarge$Beta<- RollbetaLafarge$LafargeHolcim
RollbetaLafarge$LafargeHolcim <- NULL
Rollbeta_Lag1_Lafarge <- lag.xts(RollbetaLafarge, k = +1)
Rollbeta_Real_Lafarge <-merge(RollbetaLafarge, Rollbeta_Lag1_Lafarge)
Rollbeta_Real_Lafarge$past_beta <- Rollbeta_Real_Lafarge$Beta.1
Rollbeta_Real_Lafarge$Beta.1 <- NULL

Model_Lafarge <- lm(Rollbeta_Real_Lafarge$Beta~Rollbeta_Real_Lafarge$past_beta)
stargazer(Model_Lafarge,type = "text")


# Swisscom

Swisscom<-Allmonthlyreturn$Swisscom['1998-11-30/2021-02-26']
Market_Swisscom<-Marketreturn['1998-11-30/2021-02-26']
rf_3<-SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD['1998-11-30/2021-02-26']

RollbetaSwisscom<-rollapply(data=Swisscom, width = 60,FUN=CAPM.beta, Rb=Market_Swisscom, Rf=rf_3,by=1, align = "right", by.column = TRUE)

RollbetaSwisscom$Beta<- RollbetaSwisscom$Swisscom
RollbetaSwisscom$Swisscom <- NULL
Rollbeta_Lag1_Swisscom <- lag.xts(RollbetaSwisscom, k = +1)
Rollbeta_Real_Swisscom <-merge(RollbetaSwisscom, Rollbeta_Lag1_Swisscom)
Rollbeta_Real_Swisscom$past_beta <- Rollbeta_Real_Swisscom$Beta.1
Rollbeta_Real_Swisscom$Beta.1 <- NULL

Model_Swisscom <- lm(Rollbeta_Real_Swisscom$Beta~Rollbeta_Real_Swisscom$past_beta)
stargazer(Model_Swisscom,type = "text")


# Predicting Betas

# Build a table
Predicted_betas <- matrix(NA, nrow =  4, ncol = 1, dimnames = list(c("Adecco", "Credit Suisse", "LafargeHolcim", "Swisscom"), c("B_t+1")))


Predicted_betas["Adecco",1] <- Model_Adecco$coefficients[1] + Model_Adecco$coefficients[2] * last(Rollbeta_Real_Adecco$Beta)
Predicted_betas["Credit Suisse",1] <- Model_CS$coefficients[1] + Model_CS$coefficients[2] * last(Rollbeta_Real_CS$Beta)
Predicted_betas["LafargeHolcim",1] <- Model_Lafarge$coefficients[1] + Model_Lafarge$coefficients[2] * last(Rollbeta_Real_Lafarge$Beta)
Predicted_betas["Swisscom",1] <- Model_Swisscom$coefficients[1] + Model_Swisscom$coefficients[2] * last(Rollbeta_Real_Swisscom$Beta)

print(Predicted_betas)

###############################################################################################################
###############################################################################################################

### Section 5.3 - Beta as Risk Measure

## 5.3.1 - Mean Return an Standard Deveation of the five Portfolios

# Calculating daily returns
Dailyreturn <- Return.calculate(prices = DailyStocks , method = 'discrete')
Dailyreturn <- as.matrix(Dailyreturn["1994-04-29/"])

Daily_Beta <- as.matrix(DailyBetas)

# check if they are the same no. of observation
nrow(Dailyreturn) == nrow(Daily_Beta)

# Create empty vectors with lenght = number of periods
P_Returns_P1_daily <- as.matrix(rep(NA, dim(Daily_Beta)[1]-1))
P_Returns_P2_daily <- as.matrix(rep(NA, dim(Daily_Beta)[1]-1))
P_Returns_P3_daily <- as.matrix(rep(NA, dim(Daily_Beta)[1]-1))
P_Returns_P4_daily <- as.matrix(rep(NA, dim(Daily_Beta)[1]-1))
P_Returns_P5_daily <- as.matrix(rep(NA, dim(Daily_Beta)[1]-1))

# Create the portfolio and the portfolio returns

for (j in 1:dim(Daily_Beta)[1]-1){ 
  SUB_daily <- Daily_Beta[j,]
  
  P1_P_daily <- subset (SUB_daily, subset = SUB_daily < quantile(SUB_daily, c(0.2), na.rm=TRUE))
  P2_P_daily <- subset (SUB_daily, subset = SUB_daily < quantile(SUB_daily, c(0.4), na.rm=TRUE)
                        & SUB_daily >= quantile(SUB_daily, c(0.2), na.rm=TRUE))
  P3_P_daily <- subset (SUB_daily, subset = SUB_daily < quantile(SUB_daily, c(0.6), na.rm=TRUE)
                        & SUB_daily >= quantile(SUB_daily, c(0.4), na.rm=TRUE))
  P4_P_daily <- subset (SUB_daily, subset = SUB_daily < quantile(SUB_daily, c(0.8), na.rm=TRUE)
                        & SUB_daily >= quantile(SUB_daily, c(0.6), na.rm=TRUE))
  P5_P_daily <- subset (SUB_daily, subset = SUB_daily >= quantile(SUB_daily, c(0.8), na.rm=TRUE))
  
  P_Returns_P1_daily[j] <- mean(Dailyreturn[j+1, names(P1_P_daily)], na.rm=TRUE)
  P_Returns_P2_daily[j] <- mean(Dailyreturn[j+1, names(P2_P_daily)], na.rm=TRUE)
  P_Returns_P3_daily[j] <- mean(Dailyreturn[j+1, names(P3_P_daily)], na.rm=TRUE)
  P_Returns_P4_daily[j] <- mean(Dailyreturn[j+1, names(P4_P_daily)], na.rm=TRUE)
  P_Returns_P5_daily[j] <- mean(Dailyreturn[j+1, names(P5_P_daily)], na.rm=TRUE)
}

# Preparing a data table
Portfolio <- matrix(data = NA, nrow = 5, ncol = 5,
                         dimnames = list(c("Portfolio 1", "Portfolio 2", "Portfolio 3", "Portfolio 4", "Portfolio 5"),
                                         c("Daily Mean Return", "Annualized Mean Return", "Daily Std. Deviation", "Annualized Std. Deviation", "Sharpe-Ratio")))

# Calculate the daily mean return of the portfolios
Portfolio[1,"Daily Mean Return"] <- mean(P_Returns_P1_daily, na.rm=TRUE)
Portfolio[2,"Daily Mean Return"] <- mean(P_Returns_P2_daily, na.rm=TRUE)
Portfolio[3,"Daily Mean Return"] <- mean(P_Returns_P3_daily, na.rm=TRUE) 
Portfolio[4,"Daily Mean Return"] <- mean(P_Returns_P4_daily, na.rm=TRUE)
Portfolio[5,"Daily Mean Return"] <- mean(P_Returns_P5_daily, na.rm=TRUE)

# Annualized mean return of the five portfolios
Portfolio[,"Annualized Mean Return"] <- (1 + Portfolio[,"Daily Mean Return"]) ^252 - 1

# Calculate the mean daily standard deviation of the five portfolios
Portfolio[1,"Daily Std. Deviation"] <- mean(rollapplyr(P_Returns_P1_daily, 252, sd, fill = NA), na.rm = TRUE) 
Portfolio[2,"Daily Std. Deviation"] <- mean(rollapplyr(P_Returns_P2_daily, 252, sd, fill = NA), na.rm = TRUE) 
Portfolio[3,"Daily Std. Deviation"] <- mean(rollapplyr(P_Returns_P3_daily, 252, sd, fill = NA), na.rm = TRUE) 
Portfolio[4,"Daily Std. Deviation"] <- mean(rollapplyr(P_Returns_P4_daily, 252, sd, fill = NA), na.rm = TRUE) 
Portfolio[5,"Daily Std. Deviation"] <- mean(rollapplyr(P_Returns_P5_daily, 252, sd, fill = NA), na.rm = TRUE) 

# Annualized mean standard deviation of the five portfolios
Portfolio[,"Annualized Std. Deviation"] <- Portfolio[,"Daily Std. Deviation"] * sqrt(252)

# Portfolios Sharpe ratio
riskfree <-  mean(SwissBond_M$SWISS.CONFEDERATION.BOND.1.YEAR...RED..YIELD["1994-04-29/"])

Portfolio[, "Sharpe-Ratio"] <- (Portfolio[, "Annualized Mean Return"] - riskfree) / Portfolio[,"Annualized Std. Deviation"]

print(Portfolio)


#################################################

## 5.3.2 - Plot of the five Portfolios

# Create a single xts including the 5 portfolios' raw returns
Portfolios <- cbind(P_Returns_P1_daily, P_Returns_P2_daily,P_Returns_P3_daily, P_Returns_P4_daily, P_Returns_P5_daily)
colnames(Portfolios) <- c('Portfolio 1', 'Portfolio 2', 'Portfolio 3', 'Portfolio 4', 'Portfolio 5')
Portfolios <- xts(Portfolios,order.by = index(DailyBetas[-1]))

# Plot
colors<-c(1,2,3,4,5)
chart.CumReturns(R = Portfolios, wealth.index = TRUE, legend.loc  = "topleft",
                 begin = c("first","axis"), main="Wealth Index of the Five Different Portfolios",col=colors)








