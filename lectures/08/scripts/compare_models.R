library(forecast)

load("data/ausbeer.rda")

beer <- window(ausbeer, start=1992, end=2006-.1)
plot(beer)

# Среднее значение предыдущих наблюдений
# h=5 - прогноз делается на ближайшие 5 наблюдений.  
forecast.1 <- meanf(beer, h=5)
plot(forecast.1)
# Наивный прогноз: по последнему значению ряда
forecast.2 <- naive(beer, h=5)
plot(forecast.2)
# Наивный прогноз с учетом сезонности:
# прогноз равен последнему наблюдаемому значению того же сезона.
forecast.3 <- snaive(beer, h=5)
plot(forecast.3)
# Метод Холта-Уинтерса
forecast.4 <- hw(beer, h=5)
plot(forecast.4)

beer_test <- window(ausbeer, start=2006)

accuracy(forecast.1, beer_test)
accuracy(forecast.2, beer_test)
accuracy(forecast.3, beer_test)
accuracy(forecast.4, beer_test)

# ARIMA
arima_model <- auto.arima(beer, stepwise = F, approximation = F)
forecast.5 <- forecast(arima_model, h=5)
plot(forecast.5)

accuracy(forecast.5, beer_test)
