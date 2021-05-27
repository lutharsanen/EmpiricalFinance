#install packages used for this assignment
install.packages('xts')
install.packages('PerformanceAnalytics')
install.packages('psych')
install.packages("roll")
install.packages("rollRegres")
library(xts)
library(PerformanceAnalytics)
library(psych)
library(roll)
library(rollRegres)

setwd("~/Desktop/emp fin/Assignments/Assignment 4")
###

# read in the datasets and create time series
adj_stock_prices <- read.delim("A4_dataset_01.txt",sep = "\t", header = TRUE, na = "NA")
date_monthly <- as.Date(adj_stock_prices$Date,tryFormats=c("%d.%m.%Y"))
adj_stock_prices_ts <- xts(adj_stock_prices[,-1], order.by = date_monthly)


book_values_stock <- read.delim("A4_dataset_02.txt",sep = "\t", header = TRUE, na = "NA")
date_monthly <- as.Date(book_values_stock$Date,tryFormats=c("%d.%m.%Y"))
book_values_stock_ts <- xts(book_values_stock[,-1], order.by = date_monthly)


numb_of_shares <- read.delim("A4_dataset_03.txt",sep = "\t", header = TRUE, na = "NA")
date_monthly <- as.Date(numb_of_shares$Date,tryFormats=c("%d.%m.%Y"))
numb_of_shares_ts <- xts(numb_of_shares[,-1], order.by = date_monthly)


unadj_stock_prices <- read.delim("A4_dataset_04.txt",sep = "\t", header = TRUE, na = "NA")
date_monthly <- as.Date(unadj_stock_prices$Date,tryFormats=c("%d.%m.%Y"))
unadj_stock_prices_ts <- xts(unadj_stock_prices[,-1], order.by = date_monthly)
 

ann_rf <- read.delim("A4_dataset_05.txt",sep = "\t", header = TRUE, na = "NA")
date_monthly <- as.Date(ann_rf$Date,tryFormats=c("%d.%m.%Y"))
ann_rf_ts <- xts(ann_rf[,-1], order.by = date_monthly)


fact_returns <- read.delim("A4_dataset_06.txt",sep = "\t", header = TRUE, na = "NA")
date_monthly <- as.Date(fact_returns$Date,tryFormats=c("%d.%m.%Y"))
fact_returns_ts <- xts(fact_returns[,-1], order.by = date_monthly)

# 5.1
# Size

# 5.1.1
### returns
monthly_total_returns <- Return.calculate(adj_stock_prices_ts, method = "discrete")
# View(monthly_total_returns['1991-02-01'])

### marekt cap

# calculate monthly market cap
monthly_market_caps <- numb_of_shares_ts * unadj_stock_prices_ts

# 5.1.2
# get monthly median market cap
monthly_median_cap <- apply(monthly_market_caps, 1, median, na.rm=TRUE)
# head(monthly_median_cap)

#assign S to values below median and B to values above median
PF_assign_S_B <- apply(monthly_market_caps, 2, function(x) ifelse(x <= monthly_median_cap, 'S', 'B'))

# add value of 1 to values below median and NA otherwise
S_PF <- apply(monthly_market_caps, 2, function(x) ifelse(x <= monthly_median_cap, 1, NA))
# add value of 1 to values above median and NA otherwise
B_PF <- apply(monthly_market_caps, 2, function(x) ifelse(x <= monthly_median_cap, NA, 1))


# 5.1.3 
# calculate the two separate portfolio returns
S_returns <- rowMeans(monthly_total_returns[2:dim(monthly_total_returns)[1],] * S_PF[1:dim(monthly_total_returns)[1]-1,], na.rm = TRUE)
B_returns <- rowMeans(monthly_total_returns[2:dim(monthly_total_returns)[1],] * B_PF[1:dim(monthly_total_returns)[1]-1,], na.rm = TRUE)
# dim(S_PF)

# calculate the zero-investment portfolio monthly returns 
SMB <- as.data.frame(S_returns - B_returns)
colnames(SMB) <- c("SMB")

#define as time series
date_monthly <- as.Date(adj_stock_prices[2:dim(monthly_total_returns)[1],]$Date, "%d.%m.%Y")
SMB_PF_returns<- xts(SMB, order.by = date_monthly)

# calculate Annualized mean return
SMB_ann_mean_return <- (mean(SMB_PF_returns['1991-02-01/2019-12-01']$SMB)+1)^12-1
SMB_ann_mean_return

# calculate sharpe ratio
mean_rf <- mean(ann_rf_ts['1991-02-01/2019-12-01']/100)
std_SMB <- StdDev.annualized(SMB_PF_returns['1991-02-01/2019-12-01']$SMB)
std_SMB
SMB_sharpe <- (SMB_ann_mean_return - mean_rf) / std_SMB[1]
SMB_sharpe

# Cumulative Return Plot
SMB_cum_prod <- cumprod(1+SMB_PF_returns['1991-02-01/2019-12-01']$SMB)
date <- index(SMB_PF_returns['1991-02-01/2019-12-01'])

plot(x=date, y=SMB_cum_prod, type = "l", lty = 1, lwd =1, col = "black", 
     cex.axis = 1.5, cex.lab = 1.5, ylab = "Cumulative Returns", xlab = "Date", main= "Cum. return of SMB PF over time")
legend("topright", legend = c("Cum. return"), lty = 1, lwd = 2, col = c("black"))


# Value

# 5.1.4 
# calculate book-to-market values of the companies
book_values <- book_values_stock_ts[1:(dim(book_values_stock_ts)[1]-6),]

# check for negative book values and remove them / set them to 0
neg <- apply(book_values, 1, function(row) any(row < 0, na.rm = TRUE))
table(neg)['TRUE']
book_values <- apply(book_values, 2, function(x) ifelse(x <= 0, NA, x))

date_monthly <- as.Date(unadj_stock_prices[7:dim(book_values_stock_ts)[1],]$Date, "%d.%m.%Y")
book_values <- xts(book_values, order.by = date_monthly)
book_to_market <- book_values / monthly_market_caps[7:dim(book_values_stock_ts)[1],] 
# head(book_to_market)


#5.1.5

#Asness, and Frazzini (2013) pointed out that standard HML has a huge loss of information.
#The core of HML is the calculation of book-to-market value ratio.
#BM = BV/ MV
#In the formula, BV and MV represent the current book value and market value of the previous annual report.
#Since last year's annual report data can only be used after all listed companies have disclosed their annual reports at the end of June each year, 
#Fama-French's HML definition has almost always used lagging data, and the lag period of data can reach up to 18 months.

#For example, if we want to calculate the BM value today, since the 2021 annual report has not yet been fully disclosed, 
#the 2020 annual report and the market value of the last trading day in 2020 need to be used. Obviously, 
#there is a lag of several months. In such a long period of time, the stock price may fluctuate significantly, resulting in huge changes in the book-to-market ratio.
#One method is that the numerator still uses the book value of the previous year's annual report, but the denominator uses the latest market value data.
#Asness, Clifford, and Andrea Frazzini. ??The Devil in HML??s Details.?? The Journal of Portfolio Management 39.4 (2013): 49-68.


#5.1.6
#The price-to-earnings ratio is one of the commonly used methods to judge the value of a company. 
#It is used to measure the time it takes for investors to recover the cost through the current stock price investment, 
#under the condition of the same earnings per share.
#In general, the lower the P/E ratio, the better.

#The price-to-book ratio (P/B) is also a commonly used valuation method in the market. 
#It divides the stock price by the net assets per share.
#Stocks with low price-to-book ratios have more safety margins than stocks with high net ratios.

#Fama and French decided to use the book-to-market value ratio as an indicator to measure the value of each stock: 
#stocks with a higher book value are more suitable for value investment because the calculated value (book value) is relatively higher than the market value (stock price).
#Fama and French argue that the value factor defined by the book-to-market ratio helps explain the return of securities while providing a premium that is higher than the market return.
#The persistence of the value premium makes it the most popular investment strategy ever.

#5.1.7
# get monthly median book-to-market value
monthly_mean_btm <- apply(book_to_market, 1, median, na.rm=TRUE)
# head(monthly_mean_btm)

# assign L to values below median and H to values above median
PF_assign_L_H <- apply(book_to_market, 2, function(x) ifelse(x <= monthly_mean_btm, 'L', 'H'))

# add value of 1 to values below median and NA otherwise
L_PF <- apply(book_to_market, 2, function(x) ifelse(x <= monthly_mean_btm, 1, NA))
# add value of 1 to values above median and NA otherwise
H_PF <- apply(book_to_market, 2, function(x) ifelse(x <= monthly_mean_btm, NA, 1))

#5.1.8
# determine in which portfolio the big four appear on January 1998 and on January 2008
companies <- c("NESN", "ROG", "UBSG", "NOVN")
dates <- c("1998-01-01", "2008-01-01")
PF_assign_L_H[dates, companies]

#5.1.9
# calculate the two separate portfolio returns
L_returns <- rowMeans(monthly_total_returns[(2+6):dim(monthly_total_returns)[1],] * L_PF[1:(dim(monthly_total_returns)[1]-(1+6)),], na.rm = TRUE)
H_returns <- rowMeans(monthly_total_returns[(2+6):dim(monthly_total_returns)[1],] * H_PF[1:(dim(monthly_total_returns)[1]-(1+6)),], na.rm = TRUE)

# calculate the zero-investment portfolio monthly returns 
LMH <- as.data.frame(L_returns - H_returns)
colnames(LMH) <- c("LMH")
# define as time series
date_monthly <- as.Date(adj_stock_prices[(2+6):dim(monthly_total_returns)[1],]$Date, "%d.%m.%Y")
LMH_PF_returns<- xts(LMH, order.by = date_monthly)

# calculate Annualized mean return
LMH_ann_mean_return <- (mean(LMH_PF_returns['1991-02-01/2019-12-01']$LMH)+1)^12-1
LMH_ann_mean_return

# calculate sharpe ratio
std_LMH <- StdDev.annualized(LMH_PF_returns['1991-02-01/2019-12-01']$LMH)
std_LMH
LMH_sharpe <- (LMH_ann_mean_return - mean_rf) / std_LMH[1]
LMH_sharpe

# Cumulative Return Plot
LMH_cum_prod <- cumprod(1+LMH_PF_returns['1991-02-01/2019-12-01']$LMH)


plot(x=date, y=LMH_cum_prod, type = "l", lty = 1, lwd =1, col = "blue", 
     cex.axis = 1.5, cex.lab = 1.5, ylab = "Cumulative Returns", xlab = "Date", main= "Cum. return of LMH PF over time")
legend("topright", legend = c("LMH"), lty = 1, lwd = 2, col = c("blue"))


# Momentum
# head(monthly_total_returns)

#The reason why relates to the short-term reversal effect associated with momentum.
#There is an academic finding that short-term momentum actually has a reversal affect, 
#whereby the previous winners (measured over the past month) do poorly the next month, 
#while the previous losers (measured over the past month) do well the next month. 
#Researchers often argue that this is due to microstructure issues. 
#Thus, most academics ignore the previous month's return in the momentum calculation, 
#and we also do this in order to eliminate this short-term reversal effect when implementing the strategy.  
#It should be noted, however, that including the previous month's returns has a marginal affect on the performance of momentum.

#With momentum,an investor is able to utilize market volatility to the investor??s advantage. 
#The investor is able to capitalize on volatile market trends by following, 
#or avoiding, market movements as they trend upwards or downwards.

#5.1.14

#By including both listed and delisted stocks, the present study
#mitigates the issue of survivorship bias and provides a clearer picture of the momentum pattern in stock returns.
#Survivorship bias arises when the calculation of momentum profits
#includes only surviving listed firms while omitting delisted firms that have not
#survived. Omitting delisted firms causes the calculated momentum profits to be
#upward biased in comparison to the true momentum profits of all firms, listed
#and delisted. (Tee, L.-T. et al. (2019))
#Eisdorfer (2008) finds that a significant portion of momentum
#profit, about 40%, is generated by delisted stocks, most of which are bankrupt firms.

#References:
#Tee, L.-T. et al. (2019) Do momentum strategies perform better for Islamic stocks than for conventional stocks across market states? Ekonomski anali. [Online] 64 (221), 107?C129.
#Eisdorfer, A. (2008) Delisted firms and momentum profits. Journal of financial markets (Amsterdam, Netherlands). [Online] 11 (2), 160?C179.


date_monthly_mom <- as.Date(adj_stock_prices$Date, tryFormats=c("%d.%m.%Y"))
# length(date_monthly_mom)
date_monthly_mom <- date_monthly_mom[13:dim(adj_stock_prices)[1]]
# length(date_monthly_mom)
t_12 <- adj_stock_prices_ts[1:(dim(adj_stock_prices_ts)[1]-12)]
t_12<- xts(t_12, order.by = date_monthly_mom)
t_1 <- adj_stock_prices_ts[12:(dim(adj_stock_prices_ts)[1]-1)]
t_1 <- xts(t_1, order.by = date_monthly_mom)

momentum <- (t_1 - t_12)/t_12

monthly_mean_momentum <- apply(momentum, 1, median, na.rm=TRUE)

PF_assign_D_U <- apply(momentum, 2, function(x) ifelse(x <= monthly_mean_momentum, 'D', 'U'))

D_PF <- apply(momentum, 2, function(x) ifelse(x <= monthly_mean_momentum, 1, NA))
U_PF <- apply(momentum, 2, function(x) ifelse(x <= monthly_mean_momentum, NA, 1))

D_returns <- rowMeans(monthly_total_returns[13:dim(monthly_total_returns)[1],] * D_PF, na.rm = TRUE)
U_returns <- rowMeans(monthly_total_returns[13:dim(monthly_total_returns)[1],] * U_PF, na.rm = TRUE)


UMD <- as.data.frame(U_returns - D_returns)
colnames(UMD) <- c("UMD")
UMD_PF_returns<- xts(UMD, order.by = date_monthly_mom)

UMD_ann_mean_return <- (mean(UMD_PF_returns['1991-02-01/2019-12-01']$UMD)+1)^12-1
UMD_ann_mean_return


std_UMD <- StdDev.annualized(UMD_PF_returns['1991-02-01/2019-12-01']$UMD)
std_UMD
UMD_sharpe <- (UMD_ann_mean_return - mean_rf) / std_UMD[1]
UMD_sharpe

# Cumulative Return Plot
UMD_cum_prod <- cumprod(1+UMD_PF_returns['1991-02-01/2019-12-01']$UMD)


plot(x=date, y=UMD_cum_prod, type = "l", lty = 1, lwd =1, col = "green", 
     cex.axis = 1.5, cex.lab = 1.5, ylab = "Cumulative Returns", xlab = "Date", main= "Cum. return of UMD PF over time")
legend("topright", legend = c("UMD"), lty = 1, lwd = 2, col = c("green"))



# Plot of SMB, LMH and UMD portfolios

plot(x=date, y=SMB_cum_prod,ylim=c(0,5.5), type = "l", lty = 1, lwd =1, col = "black", 
     cex.axis = 1.5, cex.lab = 1.5, ylab = "Cumulative Returns", xlab = "Date", main= "Cum. returns over time")
lines(date,LMH_cum_prod , lty = 1, lwd = 1, col = "blue")
lines(date,UMD_cum_prod , lty = 1, lwd = 1, col = "green")
legend("topleft", legend = c("SMB", "LMH", "UMD"), lty = 1, lwd = 2, col = c("black","blue", "green"))



#SLD 
dim(S_PF)
dim(L_PF)
dim(D_PF)

# define the time frames 1991-01-01/ 2019-12-01 (one period more because of sorting of one month before later)
S <- S_PF[14:dim(S_PF)[1],]
B <- B_PF[14:dim(B_PF)[1],]

L <- L_PF[8:dim(L_PF)[1],]
H <- H_PF[8:dim(H_PF)[1],]

D <- D_PF[2:dim(D_PF)[1],]
U <- U_PF[2:dim(U_PF)[1],]


# create subportfolios
SLD_portfolio <- S + L + D
SLD <- apply(SLD_portfolio, 2, function(x) ifelse(x==3, 1))
SLD[is.na(SLD)] <- 0

#SLU
SLU_portfolio <- S + L + U
SLU <- apply(SLU_portfolio, 2, function(x) ifelse(x==3, 1))
SLU[is.na(SLU)] <- 0

#SHD
SHD_portfolio <- S + H + D
SHD <- apply(SHD_portfolio, 2, function(x) ifelse(x==3, 1))
SHD[is.na(SHD)] <- 0

#SHU
SHU_portfolio <- S + H + U
SHU <- apply(SHU_portfolio, 2, function(x) ifelse(x==3, 1))
SHU[is.na(SHU)] <- 0

#BLD
BLD_portfolio <- B + L + D
BLD <- apply(BLD_portfolio, 2, function(x) ifelse(x==3, 1))
BLD[is.na(BLD)] <- 0

#BLU
BLU_portfolio <- B + L + U
BLU <- apply(BLU_portfolio, 2, function(x) ifelse(x==3, 1))
BLU[is.na(BLU)] <- 0

#BHD
BHD_portfolio <- B + H + D
BHD <- apply(BHD_portfolio, 2, function(x) ifelse(x==3, 1))
BHD[is.na(BHD)] <- 0

#BHU
BHU_portfolio <- B + H + U
BHU <- apply(BHU_portfolio, 2, function(x) ifelse(x==3, 1))
BHU[is.na(BHU)] <- 0

# date frame for portfolio
date_monthly_pfs <- date_monthly[8:354]

# create returns of subportfolios and mean size
SLD_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * SLD[1:(dim(SLD)[1]-1),], na.rm = TRUE)
SLD_returns <- xts(SLD_returns, order.by = date_monthly_pfs)
colnames(SLD_returns) <- c("SLD_returns")
mean(rowSums(SLD, na.rm = TRUE))

SLU_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * SLU[1:(dim(SLU)[1]-1),], na.rm = TRUE)
SLU_returns <- xts(SLU_returns, order.by = date_monthly_pfs)
colnames(SLU_returns) <- c("SLU_returns")
mean(rowSums(SLU, na.rm = TRUE))


SHD_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * SHD[1:(dim(SHD)[1]-1),], na.rm = TRUE)
SHD_returns <- xts(SHD_returns, order.by = date_monthly_pfs)
colnames(SHD_returns) <- c("SHD_returns")
mean(rowSums(SHD, na.rm = TRUE))

SHU_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * SHU[1:(dim(SHU)[1]-1),], na.rm = TRUE)
SHU_returns <- xts(SHU_returns, order.by = date_monthly_pfs)
colnames(SHU_returns) <- c("SHU_returns")
mean(rowSums(SHU, na.rm = TRUE))


BLD_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * BLD[1:(dim(BLD)[1]-1),], na.rm = TRUE)
BLD_returns <- xts(BLD_returns, order.by = date_monthly_pfs)
colnames(BLD_returns) <- c("BLD_returns")
mean(rowSums(BLD, na.rm = TRUE))

BLU_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * BLU[1:(dim(BLU)[1]-1),], na.rm = TRUE)
BLU_returns <- xts(BLU_returns, order.by = date_monthly_pfs)
colnames(BLU_returns) <- c("BLU_returns")
mean(rowSums(BLU, na.rm = TRUE))


BHD_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * BHD[1:(dim(BHD)[1]-1),], na.rm = TRUE)
BHD_returns <- xts(BHD_returns, order.by = date_monthly_pfs)
colnames(BHD_returns) <- c("BHD_returns")
mean(rowSums(BHD, na.rm = TRUE))

BHU_returns <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * BHU[1:(dim(BHU)[1]-1),], na.rm = TRUE)
BHU_returns <- xts(BHU_returns, order.by = date_monthly_pfs)
colnames(BHU_returns) <- c("BHU_returns")
mean(rowSums(BHU, na.rm = TRUE))



# 5.2.3 creating factor mimicking PF
SMB_PF <- 1/4*((SHU-BHU)+(SHD-BHD)+(SLU-BLU)+(SLD-BLD))
HML_PF <- 1/4*((SHU-SLU)+(SHD-SLD)+(BHU-BLU)+(BHD-BLD))
MOM_PF <- 1/4*((SHU-SHD)+(SLU-SLD)+(BHU-BHD)+(BLU-BLD))


# 5.3.4 for table completion

# returns
SMB_PF_return <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * SMB_PF[1:(dim(SMB_PF)[1]-1),],na.rm=TRUE)
SMB_PF_return <- xts(SMB_PF_return, order.by = date_monthly_pfs)
colnames(SMB_PF_return) <- c("SMB_PF_return")

HML_PF_return <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * HML_PF[1:(dim(HML_PF)[1]-1),],na.rm=TRUE)
HML_PF_return <- xts(HML_PF_return, order.by = date_monthly_pfs)
colnames(HML_PF_return) <- c("HML_PF_return")

MOM_PF_return <- rowMeans(monthly_total_returns[15:(dim(monthly_total_returns)[1]),] * MOM_PF[1:(dim(MOM_PF)[1]-1),],na.rm=TRUE)
MOM_PF_return <- xts(MOM_PF_return, order.by = date_monthly_pfs)
colnames(MOM_PF_return) <- c("MOM_PF_return")

# SMB
SMB_ann_mean_PF_return <- (mean(SMB_PF_return)+1)^12 -1
SMB_ann_mean_PF_return
std_SMB_PF <- StdDev.annualized(SMB_PF_return$SMB_PF_return)
std_SMB_PF
SMB_PF_sharpe <- (SMB_ann_mean_PF_return - mean_rf) / std_SMB_PF[1]
SMB_PF_sharpe

# HML
HML_ann_mean_PF_return <- (mean(HML_PF_return)+1)^12 -1
HML_ann_mean_PF_return
std_HML_PF <- StdDev.annualized(HML_PF_return)
std_HML_PF
HML_PF_sharpe <- (HML_ann_mean_PF_return - mean_rf) / std_HML_PF[1]
HML_PF_sharpe

# MOM
MOM_ann_mean_PF_return <- (mean(MOM_PF_return)+1)^12 -1
MOM_ann_mean_PF_return
std_MOM_PF <- StdDev.annualized(MOM_PF_return)
std_MOM_PF
MOM_PF_sharpe <- (MOM_ann_mean_PF_return - mean_rf) / std_MOM_PF[1]
MOM_PF_sharpe

# Cumulative returns for plot
SMB_PF_cum_prod <- cumprod(1+SMB_PF_return)
HML_PF_cum_prod <- cumprod(1+HML_PF_return)
MOM_PF_cum_prod <- cumprod(1+MOM_PF_return)

plot(x=date_monthly_pfs, y=SMB_PF_cum_prod, ylim=c(0.9,1.5), type = "l", lty = 1, lwd =1, col = "black", 
     cex.axis = 1.5, cex.lab = 1.5, ylab = "Cumulative Returns", xlab = "Date", main= "Cum. returns over time")
lines(date_monthly_pfs,HML_PF_cum_prod , lty = 1, lwd = 1, col = "blue")
lines(date_monthly_pfs,MOM_PF_cum_prod , lty = 1, lwd = 1, col = "green")
legend("topleft", legend = c("SMB", "HML", "MOM"), lty = 1, lwd = 2, col = c("black","blue", "green"))


### 5.3.1











dim(SMB_PF[1:(dim(SMB_PF)[1]-1),])

dim(monthly_total_returns[15:(dim(monthly_total_returns)[1])])

length(SMB_PF_return)

View(BHU_returns)


SHU_results <- Return.portfolio(SHU_returns, verbose=T)
bop <- SHU_results$BOP.Weight
eop <- SHU_results$EOP.Weight



sum(bop-eop)

data(edhec)
results <- Return.portfolio(edhec,rebalance_on="months",verbose=T)

bop <- results$BOP.Weight #beginning of period weights

eop <- results$EOP.Weight #end of period weights

abs(bop-eop)

length(results)




# 5.2.2
# Mean size (number of companies)
SLD_size <- rowSums(SLD, na.rm = TRUE)
SLD_meansize <- mean(SLD_size)
SLD_meansize

SLU_size <- rowSums(SLU, na.rm = TRUE)
SLU_meansize <- mean(SLU_size)
SLU_meansize


SHD_size <- rowSums(SHD, na.rm = TRUE)
SHD_meansize <- mean(SHD_size)
SHD_meansize

SHU_size <- rowSums(SHU, na.rm = TRUE)
SHU_meansize <- mean(SHU_size)
SHU_meansize

BLD_size <- rowSums(BLD, na.rm = TRUE)
BLD_meansize <- mean(BLD_size)
BLD_meansize

BLU_size <- rowSums(BLU, na.rm = TRUE)
BLU_meansize <- mean(BLU_size)
BLU_meansize

BHD_size <- rowSums(BHD, na.rm = TRUE)
BHD_meansize <- mean(BHD_size)
BHD_meansize

BHU_size <- rowSums(BHU, na.rm = TRUE)
BHU_meansize <- mean(BHU_size)
BHU_meansize

###Playing around
# Average Turnover (wrong)
rowSums(S, na.rm = TRUE)
SLD_turnover <- (rowSums(S, na.rm = TRUE) + rowSums(L, na.rm = TRUE))/ rowSums(D, na.rm = TRUE)
View(SLD_turnover)
mean(SLD_turnover)

#5.2.3
# factor-mimicking-portfolios (wrong)
SMB <- 1/4*((SHU-BHU)+(SHD-BHD)+(SLU-BLU)+(SLD-BLD))
HML <- as.data.frame(1/4*((SHU-SLU)+(SHD-SLD)+(BHU-BLU)+(BHD-BLD)))
View(HML)
View(SHU)
mean(rowSums(SHU))
SMB <- SHU - BHU
View(SMB)
mean(rowSums(SMB, na.rm = FALSE))
