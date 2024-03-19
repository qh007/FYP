setwd('C:/Users/zhouq/OneDrive - Nanyang Technological University/FYP/Codes/FYP')

quaterly_data_raw <- read.csv("quaterly_data_raw.csv")
quaterly_data_raw$Date = as.Date(quaterly_data_raw$Date, format = '%m/%d/%Y')


"""
Data information
MONTH
Source: FRED - QD

TRANSFORMATIONS
1: none
2: D
3: DD
4: Log
5: Dlog
6: DDlog
"""

# SPECIFY FUNCTIONS FOR DATA TRANSFORMATION

D = function(x){c(NA,diff(x))}
DD = function(x){c(NA,NA,diff(diff(x)))}
L = function(x){c(log(x))}
DL = function(x){c(NA,diff(log(x)))}
DDL = function(x){c(NA,NA,diff(diff(log(x))))}

startDate = "1959-01-01"
endDate = "2019-12-31"
date_format = '%d/%m/%Y'

date_filtering = function(x,start_date, end_date, date_format){
  transform_header = x[1,]
  x$Date = as.Date(x$Date,format = date_format)
  x = x[x$Date >= start_date & x$Date <= end_date,]
  x[1,] = transform_header
  return(x)
}


# transformation type is in first row
transform = function(x){
  c2 = which(x[1,]==2)
  c3 = which(x[1,]==3)
  c4 = which(x[1,]==4)
  c5 = which(x[1,]==5)
  c6 = which(x[1,]==6)
  
  x1 = x[-(1),] # remove first row
  for (i in c2){
    tt = D(x1[,i]) 
    x1[,i] = tt}
  
  for (i in c3){
    tt = DD(x1[,i]) 
    x1[,i] = tt }
  
  for (i in c4){
    tt = L(x1[,i]) 
    x1[,i] = tt }
  
  for (i in c5){
    tt = DL(x1[,i]) 
    x1[,i] = tt}
  
  for (i in c6){
    tt = DDL(x1[,i]) 
    x1[,i] = tt }
  
  x = x1}

x = date_filtering(quaterly_data_raw,
                   start_date =  startDate,
                   end_date = endDate, 
                   date_format = date_format)
View(x)

data_transform = transform(x)
View(data_transform)

write.csv(data_transform, "Data_Q_transformed.csv", row.names=FALSE)


# unit root test to check that all data are stationary
# Agumented Dicky-Fullter test
# H0: not stationary vs H1: stationary --> Hence, aim to reject H0 to conclude data is stationary
library(lubridate)
library(tseries)

result <- data.frame(Variable = character(), p_value = numeric(), Stationary = character(), stringsAsFactors = FALSE)

for (col in colnames(data_transform)) {
  test_result = adf.test(na.omit(data_transform[[col]]))
  p_value = test_result$p.value
  reject = ifelse(p_value < 0.05, "Stationary", "Not Stationary")
  result = rbind(result, data.frame(Variable = col, p_value = p_value, Stationary = reject))
}
result

#output results as LATEX table
library(xtable)
xtable_result <- xtable(result[2:nrow(result),])
# Print the LaTeX code for the table with a caption and label
print(xtable_result, include.rownames = FALSE)
