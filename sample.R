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
