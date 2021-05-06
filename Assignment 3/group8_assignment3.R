# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 3")
# setwd("C:/Users/p_lae/OneDrive - Universit?t Z?rich UZH/Dokumente/Universit?t Z?rich/12. Semester/Empirical Finance/EmpiricalFinance/Assignment 3")

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
library(ggplot2)


###############
# Data import #
###############
prices <- read.delim(file = 'A3_dataset_01.txt', header = TRUE, sep = '\t', dec = '.')
SMI_monthly <- read.delim(file = 'A3_dataset_02.txt', header = TRUE, sep = '\t', dec = '.')
interest_rates <- read.delim(file = 'A3_dataset_03.txt', header = TRUE, sep = '\t', dec = '.')


# # create monthly returns for stocks and SMI
date <- as.Date(prices[,1], format = "%d.%m.%Y")
prices.ts <- xts(x = prices[,-1], order.by = date)
#View(prices.ts)
returns <- Return.calculate(prices = prices.ts, method = 'log')
# 
SMI_monthly <- xts(SMI_monthly[,-1], order.by = as.Date(SMI_monthly$Date, format = "%d.%m.%Y"))
#View(SMI_monthly)
SMI_TotRet_mon <- Return.calculate(SMI_monthly, method = "log")
# 
# # turn interest_rates into ts and divide by 100 because it is in percentages and turn into monthly rates
interest_rates <- xts(interest_rates[,-1], order.by = as.Date(interest_rates$Date,  format = "%d.%m.%Y"))
interest_rates_mon <- ((1+interest_rates/100)^(1/12)-1)

# take logs of interest rates
interest_rates_mon <- log(1+interest_rates_mon)

#################
###  Ex 5.1  ###
#################

####### 1.

# calculating excess returns by subtracting risk-free from returns 
excess_return <- Return.excess(returns, interest_rates_mon)

# calculate market premium by subtracting risk-free returns from market return
market_premium <- Return.excess(SMI_TotRet_mon, interest_rates_mon)


####### 2.

lin_reg_values <- data.frame(
        company_name = character(),
        alpha = double(),
        alpha_t_value = double(),
        beta=double(),
        beta_t_value = double())

# regress excess return over

for(i in 1:ncol(excess_return)){
        beta_regression <- summary(lm(excess_return[,i] ~ market_premium))
        company_name <- colnames(excess_return[,i])
        parsed_name <- gsub('.{5}$', '', company_name)
        new_row <- c(company_name = parsed_name, 
                     alpha = beta_regression$coefficients[1,1],
                     alpha_t_value = beta_regression$coefficients[1,3],
                     beta = beta_regression$coefficients[2,1],
                     beta_t_value = beta_regression$coefficients[2,3],
                     res_var = var(residuals(beta_regression))
                     )
        lin_reg_values<- rbind(lin_reg_values, new_row) 
}


names(lin_reg_values)[1]<-paste("company_name")
names(lin_reg_values)[2]<-paste("alpha")
names(lin_reg_values)[3]<-paste("alpha_t_value")
names(lin_reg_values)[4]<-paste("beta")
names(lin_reg_values)[5]<-paste("beta_t_value")
names(lin_reg_values)[6]<-paste("res_var")


####### 3.

mean_values <- data.frame(
        company_name = character(),
        mean = double())

for(i in 1:ncol(excess_return)){
        company_name <- colnames(excess_return[,i])
        parsed_name <- gsub('.{5}$', '', company_name)
        new_row <- c(company_name = parsed_name, 
                     mean = mean(excess_return[,i], na.rm = TRUE)
        )
        mean_values<- rbind(mean_values, new_row) 

}

names(mean_values)[1]<-paste("company_name")
names(mean_values)[2]<-paste("mean")

####### 4.

beta_ordered <- lin_reg_values[order(lin_reg_values$beta),]

top5beta <- tail(beta_ordered, 5)

low5beta <- head(beta_ordered, 5)

####### 5.



####### 6.

dat <- data.frame(
        return = excess_return$`Zurich_Insurance_Group_N > Rf`,
        market = market_premium
)

names(dat)[1]<-paste("ZurichInsurance")
names(dat)[2]<-paste("marketpremium")

ggplot(dat, aes(x=ZurichInsurance, y=market_premium)) +
        geom_point(shape=1) +
        geom_smooth(method=lm,   # Add linear regression line
                    se=FALSE) +
        labs(x = "Excess Return Zurich Insurance", y = "Excess Return SMI", title="Relationship of Zurich's Excess Return and the Market Premium") +
        scale_x_continuous(labels = scales::percent) +
        scale_y_continuous(labels = scales::percent) +
        theme(plot.title = element_text(hjust = 0.5))

#################
###  Ex 5.2  ###
#################


###### 1.

# make data frame with regression coefficients and mean excess returns
new_data <- cbind(lin_reg_values,mean_values)

#remove redundant column
new_data[,7] <- NULL

# convert columns to numeric values
new_data$beta <- as.numeric(new_data$beta)
new_data$mean <- as.numeric(new_data$mean)
new_data$alpha <- as.numeric(new_data$alpha)
new_data$alpha_t_value <- as.numeric(new_data$alpha_t_value)
new_data$beta_t_value <- as.numeric(new_data$beta_t_value)
new_data$res_var <- as.numeric(new_data$res_var)

# annualize mean excess return
new_data$ann_returns <- new_data$mean*12
new_data$ann_returns_geom <- ((new_data$mean+1)^12 -1)*100

# plot beta realized return relationship
plot(new_data$beta, new_data$ann_returns, main = "Beta Realized Return Relationship", xlab="Realized Beta", ylab="Mean Excess Return (ann.)")

plot(new_data$beta, new_data$ann_returns_geom, main = "Beta Realized Return Relationship", xlab="Realized Beta", ylab="Mean Excess Return (ann.)")


ggplot(new_data, aes(x=beta, y=ann_returns)) +
        geom_point(shape=1) +
        labs(x = "Realized Beta", y = "Mean Excess Return (annualized)", title="Beta-Return Relationship") +
        scale_y_continuous(labels = scales::percent) +
        theme(plot.title = element_text(hjust = 0.5))

###### 2.

# regress mean excess returns on betas
cross_section <- lm(new_data$mean ~ new_data$beta)
summary(cross_section)

#sample mean excess return:

mean_excess_market_return <- colMeans(market_premium, na.rm = TRUE)
mean_excess_market_return 
View(excess_market_return_ann)
excess_market_return_ann <- ((mean_excess_market_return+1)^12 -1)*100
###### 3.

#See t-values in 5.2.2


###### 4.
gamma_1 <- ((coef(cross_section)[[2]]+1)^12 -1)
gamma_1
gamma_0 <- ((coef(cross_section)[[1]]+1)^12-1)
gamma_0
riskfree_rate <- ((colMeans(interest_rates_mon)+1)^12 -1)

ggplot(new_data, aes(x=beta, y=ann_returns)) +
        geom_point(shape=1) +
        labs(x = "Realized Beta", y = "Mean Excess Return (annualized)", title="Beta Realized Return Relationship") +
        geom_abline(slope = gamma_1, intercept = gamma_0) +
        geom_abline(slope = ((mean_excess_market_return +1)^12 -1), intercept = riskfree_rate, color="red") +
        scale_y_continuous(labels = scales::percent)+
        theme(plot.title = element_text(hjust = 0.5)) 
  

###### 5.

# no code required


###### 6.
new_data$beta_sq <- new_data$beta^2
        
cross_section_expanded <- lm(new_data$mean ~ new_data$beta + new_data$beta_sq + new_data$res_var )
summary(cross_section_expanded)


#################
###  Ex 5.3  ###
#################

#no code required
