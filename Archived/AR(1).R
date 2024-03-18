setwd('C:/Users/zhouq/OneDrive - Nanyang Technological University/FYP/Codes/FYP')

# Load Data
GDP = read.table('quarter.txt', header = T, sep = ',')
colnames(GDP)[1] = 'Date' 
GDP$Date = as.Date(GDP$Date,format="%d/%m/%Y")
GDP = GDP[1:244,]
ts.plot(GDP$GDP)
View(GDP)

# Apply transformation 5 (dlog)
gdp_log = log(GDP$GDP)
gdp_log_diff = diff(gdp_log)
ts.plot(gdp_log_diff, main="dlog_GDP (Quarterly)")

# Fit a ARIMA(1,0,0)
library(forecast)
fit_AR1 = Arima(gdp_log_diff, order = c(1, 0, 0))
summary(fit_AR1)

'''
Coefficients:
ar1    mean
0.0191  0.0073
s.e.  0.0635  0.0007

sigma^2 = 0.000126:  log likelihood = 765.65
AIC=-1525.3   AICc=-1525.2   BIC=-1514.74

Training set error measures:
                        ME       RMSE         MAE       MPE     MAPE      MASE
Training set -1.157319e-06 0.01117773 0.006509911 -140.4037 253.3325 0.7847669
                    ACF1
Training set -0.00210267
'''





