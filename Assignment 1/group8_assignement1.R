# set working directory 
# setwd("~/UZH/Empirical Finance/Assignment 1")

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

