setwd('C:/Users/zhouq/OneDrive - Nanyang Technological University/FYP/Codes/FYP/Data')
library(lubridate)
library(tseries)

# Load Data
data = read.csv("UK.csv")
colnames(data)[colnames(data) == "Title"] <- "Date"

data = na.omit(data)

D = function(x){c(NA,diff(x))}
DD = function(x){c(NA,NA,diff(diff(x)))}
L = function(x){c(log(x))}
DL = function(x){c(NA,diff(log(x)))}
DDL = function(x){c(NA,NA,diff(diff(log(x))))}

# transform GDP Data
data$GDP = DL(data$GDP)
data$CPI = DDL(data$CPI)
data$UnE = DDL(data$UnE)
data$Ex = D(data$Ex)
data$HHC = DDL(data$HHC)
data$RPIF = DDL(data$RPIF)
data$CPA = DL(data$CPA)

# unit root test to check that all data are stationary
# Agumented Dicky-Fullter test
# H0: not stationary vs H1: stationary --> Hence, aim to reject H0 to conclude data is stationary
# for DL(GDP), p-value = 0.01 < 0.05 is small --> stationary data
# for DDL(RPI), p-value = 0.01 --> stationary

adf.test(na.omit(data$GDP))
adf.test(na.omit(data$CPI))
adf.test(na.omit(data$UnE))
adf.test(na.omit(data$Ex))
adf.test(na.omit(data$HHC))
adf.test(na.omit(data$RPIF))
adf.test(na.omit(data$CPA))

write.csv(data, "UK_transformed.csv", row.names=FALSE)
