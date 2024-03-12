setwd('C:/Users/zhouq/OneDrive - Nanyang Technological University/FYP/Codes/FYP')
library(lubridate)
library(tseries)

# Load Data
data.m = read.table('month.txt', header = T, sep = ',')
data.q = read.table('quarter.txt', header = T, sep = ',')

'''
Data information
MONTH
Source: FRED - MD
File: month.txt

TRANSFORMATIONS
1: none
2: D
3: DD
4: Log
5: Dlog
6: DDlog

M1* - 5 - INDPRO 		- IP Index
M2  - 2 - CUMFNS 		- Capacity Utilization: Manufacturing
M3  - 2 - UNRATE 		- Civilian Unemployment Rate
M4* - 5 - PAYEMS 		- All Employees: Total nonfarm
M5* - 4 - HOUST  		- Housing Starts: Total New Privately Owned
M6  - 5 - DPCERA3M086SBEA 	- Real personal consumption expenditures
M7* - 5 - RETAILx	  	- Retail and Food Services Sales
M8  - 5 - AMDMNOx 		- New Orders for Durable Goods 		
M9  - 2 - UMCSENTx		- Consumer Sentiment Index 
M10 - 6 - PPIFGS 		- PPI: Finished Goods (in file under WPSFD49207)
M11*- 5 - TWEXMMTH		- Nominal effective exchange rate US
M12*- 2 - FEDFUNDS   		- Effective Federal Funds Rate
M13 - 1 - AAAFFM     		- Moody’s Aaa Corporate Bond Minus FEDFUNDS
M14*- 1 - BAAFFM     		- Moody’s Baa Corporate Bond Minus FEDFUNDS
M15*- 1 - COMPAPFFx  		- 3-Month Commercial Paper Minus FEDFUNDS
M16 - 1 - TB3SMFFM   		- 3-Month Treasury C Minus FEDFUNDS
M17 - 1 - T10YFFM    		- 10-Year Treasury C Minus FEDFUNDS
M18 - 2 - GS1        		- 1-Year Treasury Rate
M19 - 2 - GS10       		- 10-Year Treasury Rate
M20 - 1 - GS10-TB3MS 		- 10-Year Treasury Rate - 3-Month Treasury Bill

* indicated included in the small but not the medium sized sample. 
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

# transform monthly data
x = date_filtering(data.m,
                   start_date =  startDate,
                   end_date = endDate, 
                   date_format = date_format)
data.m = transform(x)
data.m1 <- ts(data.m[,-1], start = c(1990, 1), frequency = 12)
data.q1 <- aggregate(data.m1, nfrequency = 4, mean)


# transform quarterly GDP data
x1 = date_filtering(data.q,
                   start_date = startDate,
                   end_date = endDate, 
                   date_format = date_format)
x1[,2] = DL(x1[,2])
data.q = x1
data.q[1,1]= dmy('31/03/1959')

# combine datasets
data_transform <- cbind(data.q, data.q1)
write.csv(data_transform, "Data_transform.csv", row.names=FALSE)

# unit root test to check that all data are stationary
# Agumented Dicky-Fullter test
# H0: not stationary vs H1: stationary

result <- data.frame(Variable = character(), p_value = numeric(), Reject_H0 = character(), stringsAsFactors = FALSE)

for (col in colnames(data_transform)) {
  test_result = adf.test(na.omit(data_transform[[col]]))
  p_value = test_result$p.value
  reject = ifelse(p_value < 0.05, "Yes", "No")
  result = rbind(result, data.frame(Variable = col, p_value = p_value, Reject_H0 = reject))
}
result

#output results as LATEX table
library(xtable)
xtable_result <- xtable(result[2:nrow(result),])
# Print the LaTeX code for the table with a caption and label
print(xtable_result, include.rownames = FALSE)
