# Прогнозирование продаж красных вин в Австралии (на 8 мес.)
# методом экспоненциального сглаживания

#  Шаг 0. Прочитаем данные
# Посмотрим, как выглядит файл
file.show("week_10/data/wine_Austral.dat")
wine.01 <- read.table("week_10/data/wine_Austral.dat", header=T, sep="\t")
#  Проверка: импортировали правильно?
summary(wine.01)

#  Шаг 1. Создадим временной ряд
red.wine <- ts(data = wine.01[ ,4], frequency = 12, start = c(1980, 1) )
#  Проверка: что получилось?
red.wine

#  Шаг 2. Рассмотрим график
plot(red.wine)

#  Шаг 3. Проведем экспоненциальное сглаживание
red.wine.HW <- HoltWinters(red.wine, seasonal = "mult")

#  Шаг 4. Посмотрим на результаты
plot(fitted(red.wine.HW))

#  Шаг 5. Прогнозируем на 8 мес.
red.wine.predict <- predict(red.wine.HW, n.ahead=8)
plot(red.wine.HW, red.wine.predict)
# Посмотрим доверительный интервал прогноза
forecast <- predict(red.wine.HW, n.ahead=8, 
                    prediction.interval = T, level = 0.95)
plot(red.wine.HW, forecast)

#  Шаг 6. Строим график исходного ряда, подгонки и прогноза.
#         Раскрашиваем, чтобы различать ряды
ts.plot(red.wine, red.wine.predict, red.wine.HW$fitted[,1], 
        col=c("black", "red", "orange"))
