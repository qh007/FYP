setwd('C:/Users/zhouq/OneDrive - Nanyang Technological University/FYP/Codes/FYP')

library(readr)
quaterly_data_raw <- read_csv("quaterly_data_raw.csv", col_types = cols(sasdate = col_date(format = "%m/%d/%Y")))


'''
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
'''

# SPECIFY FUNCTIONS FOR DATA TRANSFORMATION

D = function(x){c(NA,diff(x))}
DD = function(x){c(NA,NA,diff(diff(x)))}
L = function(x){c(log(x))}
DL = function(x){c(NA,diff(log(x)))}
DDL = function(x){c(NA,NA,diff(diff(log(x))))}

startDate = "1959-01-01"
endDate = "2019-12-31"
date_format = '%d/%m/%Y'

quaterly_data_raw$sasdate = format(quaterly_data_raw$sasdate, date_format)

date_filtering = function(x, start_date, end_date, date_format){
  transform_header = x[1,]
  x$Date = as.Date(x$Date,format = date_format)
  x = x[x$Date >= start_date & x$Date <= end_date,]
  x[1,] = transform_header
  return(x)}

# transformation type is in first row
transform = function(x){
  c2 = which(x[1,]==2)
  c3 = which(x[1,]==3)
  c4 = which(x[1,]==4)
  c5 = which(x[1,]==5)
  c6 = which(x[1,]==6)
  
  x1 = x[-(1),] # remove first row
  for (i in c2){tt = D(x1[,i]) 
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

x1 = date_filtering(quaterly_data_raw,
                    start_date = startDate,
                    end_date = endDate, 
                    date_format = date_format)

