#  Прогнозирование методом экспоненциального сглаживания

#  Шаг 0. Прочитаем данные
ser.g.01 <- read.table("week_10/data/series_g.csv", header=T, sep=";")
#  Проверка: импортировали правильно?
summary(ser.g.01)

#  Шаг 1. Создадим временной ряд
ser.g.02 <- ts(data = ser.g.01[ ,2], frequency = 12, start = c(1949, 1) )
#  Проверка: что получилось?
ser.g.02

#  Шаг 2. Рассмотрим график
plot(ser.g.02 )

#  Шаг 3. Проведем экспоненциальное сглаживание
ser.g.HW <- HoltWinters(ser.g.02, seasonal = "mult")

#  Шаг 4. Посмотрим на результаты
ser.g.HW$fitted
plot(ser.g.HW)
plot(fitted(ser.g.HW))

#  Шаг 5. Построена подгонка, а нужен прогноз. Прогнозируем на год
ser.g.predict <- predict(ser.g.HW, n.ahead=12)
ser.g.predict

#  Шаг 6. Строим график исходного ряда, подгонки и прогноза.
#         Раскрашиваем, чтобы различать ряды
ts.plot(ser.g.02, ser.g.predict, ser.g.HW$fitted[,1], col=c("black", "red", "orange"))
