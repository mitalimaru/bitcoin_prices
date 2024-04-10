library(readxl)
BTC_Ecotrix_Data <- read_excel("C:\\Users\\yadny\\Downloads\\BTC_Ecotrix_Data.xlsx", col_types = c("date","numeric"))
View(BTC_Ecotrix_Data)
attach(BTC_Ecotrix_Data)
library(tseries)

BTC_ts <- ts(BTC_Ecotrix_Data$`Price_Bitcoin`,frequency = 12,start = c(2017,1))
View(BTC_ts)

plot(BTC_ts)
adf.test(BTC_ts)
dev.new()
acf(BTC_ts)
pacf(BTC_ts)
diff_BTC_ts <- diff(BTC_ts)
adf.test(diff_BTC_ts)
plot(diff_BTC_ts)
par(mfrow = c(2, 1))
acf(diff_BTC_ts)
pacf(diff_BTC_ts)


install.packages("strucchange")
library(strucchange)
library(ggplot2)
BTC_model <- lm(BTC_ts~1)
summary(BTC_model)
BTC_breakpoint <- breakpoints(BTC_ts ~ 1, data = data.frame(BTC_ts = BTC_ts))
BTC_breakpoint
par(mfrow = c(1, 1))
boxplot(BTC_Ecotrix_Data$Price_Bitcoin, main = "Monthly Sensex Data")
plot(BTC_Ecotrix_Data$Date,BTC_Ecotrix_Data$Price_Bitcoin,type = "l", xlab = "Month",ylab = "Price",main = "Monthly Sensex Data")

library(forecast)
auto.arima(diff_BTC_ts)


# FOR DIFFERENCED DATA

#Model1 <- ARIMA(0,1,7)

#Model2 <- ARIMA(7,1,0)


#Testing ARIMA(0,1,7)
Model1 <- arima(BTC_Ecotrix_Data$`Price_Bitcoin`,order = c(0,1,7))
summary(Model1)
BIC(Model1)
Box.test(resid(Model5),type = c("Ljung-Box"))


## AIC value= 1412.2 and BIC value= 1430.37

Box.test(resid(Model1),type=c("Ljung-Box"))

#Since the p-value>0.05, we fail to reject the null hypothesis that the residuals are independent. Therefore, no autocorrelation.


#Testing ARIMA(7,1,0)
Model2 <- arima(BTC_Ecotrix_Data$`Price_Bitcoin`,order = c(7,1,0))
summary(Model2)
BIC(Model2)

# AIC = 1415.18 and BIC= 1433.28

Box.test(resid(Model2),type=c("Ljung-Box"))

#Since the p-value>0.05, we fail to reject the null hypothesis that the residuals are independent. Therefore, no autocorrelation is there.


# FOR UNDIFFERENCED DATA
# from ACF, we can take MA(12)


#Model3 <- ARIMA(0,0,12)
#Model4 <- ARIMA(8,0,0)

#Testing ARIMA(0,0,12)
Model3 <- arima(BTC_Ecotrix_Data$`Price_Bitcoin`,order = c(0,0,12))
summary(Model3)
BIC(Model3)

#AIC= 1444.07 and BIC= 1475.94

Box.test(resid(Model3),type=c("Ljung-Box"))

#Since the p-value>0.05, we fail to reject the null hypothesis that the residuals are independent. Therefore, no autocorrelation is there.


#Testing ARIMA(8,0,0)
Model4 <- arima(BTC_Ecotrix_Data$`Price_Bitcoin`,order = c(8,0,0))
summary(Model4)
BIC(Model4)

# AIC= 1437.11 and BIC= 1459.875


Box.test(resid(Model4),type=c("Ljung-Box"))

#Since the p-value>0.05, we fail to reject the null hypothesis that the residuals are independent. Therefore, no autocorrelation is there.


## consolidated 
## For Model1--- AIC= 1412.2 and BIC= 1430.37
## For Model2--- AIC = 1415.18 and BIC= 1433.28
## For Model3---AIC= 1444.07 and BIC= 1475.94
## For Model4--- AIC= 1437.11 and BIC= 1459.875
#We have 2 competing models Model 1 and 2. Model 1 is the best with lowest AIC and BIC and Model 2 is the second best.
# Use model 1,2

#Diagnostic checking

Resi_1 <- residuals(Model1)
Resi_2 <- residuals(Model2)

plot(Resi_1)
plot(Resi_2)
#plot of residuals of both the models are similar looking.
acf(Resi_1)
Pacf(Resi_1)
#ACF and PACF show that the error is white noise.
acf(Resi_2)
Pacf(Resi_2)
#ACF and PACF show that the error is white noise.

qqnorm(Resi_1)
qqline(Resi_1)
qqnorm(Resi_2)
qqline(Resi_2)
#qqplots also suggest the error terms are white noise.
checkresiduals(Model1)
checkresiduals(Model2)
