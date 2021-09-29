library(prophet)
library(quantmod)
library(forecast)
library("xlsx")
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)

getSymbols("AMZN", src="yahoo", from="2015-01-01", to="2019-11-29")
open_price <- subset(AMZN, select=c("AMZN.Open"))

head(open_price)
plot(open_price)

DIFFopenprice<-diff(open_price,differences = 1)
plot(DIFFopenprice)
adf.test(DIFFopenprice) #ada NA
sum(is.na(DIFFopenprice$AMZN.Open)) #hanya satu maka di omit aja
DIFFopenpriceFIX <- na.omit(DIFFopenprice)
sum(is.na(DIFFopenpriceFIX$AMZN.Close)) #sudah ga ada NA
plot(DIFFopenpriceFIX)
adf.test(DIFFopenpriceFIX)

acf(DIFFopenpriceFIX)
pacf(DIFFopenpriceFIX)
tsdisplay(DIFFopenpriceFIX) #tidak dapat model karena acf dan pacf acak, pakai eacf

eacf(DIFFopenpriceFIX) #didapat kandidat model ARIMA(0,1,1), ARIMA(1,1,0), ARIMA(2,1,1)
model1 <-Arima(open_price, order = c(0,1,1), lambda = "auto",include.drift =TRUE)
model2 <-Arima(open_price, order = c(1,1,0), lambda = "auto", include.drift =TRUE)
model3 <-Arima(open_price, order = c(2,1,1), lambda = "auto", include.drift = TRUE)
cbind(model1,model2,model3)

#atau lgsg aja pake ini
open_price <- subset(AMZN, select=c("AMZN.Open"))
modelfit <- auto.arima(open_price, lambda = "auto")
modelfit

#untuk model1/modelfit
plot(resid(modelfit))
acf( resid(modelfit), 40 )
hist(resid(modelfit), prob=TRUE, 12)
lines(density(resid(modelfit)))
qqnorm(resid(modelfit)) 
qqline(resid(modelfit))
#untuk model2
plot(resid(model2))
acf( resid(model2), 40 )
hist(resid(model2), prob=TRUE, 12)
lines(density(resid(model2)))
qqnorm(resid(model2)) 
qqline(resid(model2))

Box.test(modelfit$residuals, lag= 2, type="Ljung-Box")
Box.test(modelfit$residuals, type="Ljung-Box")

shapiro.test(modelfit$residuals)
shapiro.test(model2$residuals)

plot(as.ts(AMZN$AMZN.Open))

#forecast
price_forecast <- forecast(modelfit, h=30)
plot(price_forecast)
price_forecast
head(price_forecast$mean)
head(price_forecast$lower)
head(price_forecast$upper)

N = length(open_price)
n = 0.7*N
train = open_price[1:n, ]
test  = open_price[(n+1):N,  ]
trainarimafit <- auto.arima(train, lambda = "auto")
predlen=length(test)
trainarimafit <- forecast(trainarimafit, h=predlen)

#Plotting mean predicted values vs real data
meanvalues <- as.vector(trainarimafit$mean)
precios <- as.vector(test$AMZN.Open)
plot(meanvalues, type= "l", col= "red")
lines(precios, type = "l")
#In the red line we see our mean forecasting prediction tendency over the real close price of the stock. The tendency show a good aproach predicting the future direction of the close price.
#chekc bener ga pred kita
getSymbols("AMZN", src="yahoo", from="2019-11-29", to="2019-12-4")
open_price <- subset(AMZN, select=c("AMZN.Open"))
open_price
#oh ternyata masuk di range pred kita, wow

alpha<-1.5^(-10)
hn <- length(open_price)/(alpha*(length(open_price)+30))

lambda <- BoxCox.lambda(open_price)
dnn_pred <- nnetar(open_price, size=hn, lambda = lambda)

dnn_forecast<-forecast(dnn_pred, h=30, PI = TRUE)
plot(dnn_forecast)

head(dnn_forecast)
tail(dnn_forecast)
