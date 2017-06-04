#converting to time series
gold_price <- ts(gold_clean$open, frequency = 12)
oil_price <- ts(oil_clean$open, frequency = 12)
#all plots for gold and oil
plots.ts(gold_clean)
plot.ts(oil_clean)
#sorting data for gold
#convert date format
gold.sorted<-gold[order(as.Date(gold$Date, format="%d/%m/%y")),]
gold.sorted.ts<-ts(gold.sorted$Open, start = c(2006,0),end = c(2016,12), frequency = 12)
plot(gold.sorted.ts)
#inspect trend and seasonality
sorted.ts.d.gold <- decompose(gold.sorted.ts)
plot(sorted.ts.d.gold)
#Holtwinters filtering
holt.T.gold <- HoltWinters(gold.sorted.ts, gamma = TRUE)
plot(holt.T.gold)
#Forecast for 1 year
forecasts <- forecast.HoltWinters(holt.T.gold, h=12)
plot.forecast(forecasts)
#ARIMA model forecasts
arima.gold <- arima(gold.sorted.ts, c(0,1,0))
arima.forecasts.gold <- forecast.Arima(arima.gold, h=12)
plot(arima.forecasts.gold)
#rearranging data for oil
oil.sorted<-oil[order(as.Date(oil$Date, format="%d/%m/%y")),]
oil.sorted.ts<-ts(oil.sorted$Open, start = c(2006,0),end = c(2016,12), frequency = 12)
plot(oil.sorted.ts)
#inspect trend and seasonality for oil
sorted.ts.d.oil <- decompose(oil.sorted.ts)
plot(sorted.ts.d.oil)
#Holtwinters filtering
holt.T.oil <- HoltWinters(oil.sorted.ts, gamma = TRUE)
plot(holt.T.oil)
#Forecast for 1 year
forecasts <- forecast.HoltWinters(holt.T.oil, h=12)
plot.forecast(forecasts)
#ARIMA model forecasts
arima.oil <- arima(oil.sorted.ts, c(0,1,0))
arima.forecasts.oil <- forecast.Arima(arima.oil, h=12)
plot(arima.forecasts.oil)
#Load S&P500data and sort
sp.sorted<-sp500[rev(order(as.Date(sp500$Date, format="%d/%m/%y"))),]
#inspect trend and seasonality for sp500
sp500.sorted.ts.d <- decompose(sp.sorted.ts)
plot(sp500.sorted.ts.d)
#Holtwinters filtering
sp500.holt.T <- HoltWinters(sp.sorted.ts, gamma = TRUE)
plot(sp500.holt.T)
#ARIMA model forecasts
sp500.arima <- arima(sp.sorted.ts, c(0,1,0))
sp500.arima.forecasts <- forecast.Arima(sp500.arima, h=12)
plot(sp500.arima.forecasts)

#Quantifying variance
library(timeSeries)
vargold<-gold.sorted.ts[c(1:124,1)]
colVars(vargold)

varoil <- oil.sorted.ts[c(1:124,1)]
colVars(varoil)

varsp <- sp.sorted.ts[c(1:124,1)]
colVars(varsp)






