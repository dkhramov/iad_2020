#### Продажи розовых вин в Австралии

## 0. Загрузка/предобработка данных

au_wine <- read.table("wine_Austral.dat", sep="\t", header = T)

# Если установлена русская локаль (проверка Sys.getlocale()), 
# воспользоваться датами из au_wine$date_ будет нельзя.

# Преобразуем год и месяц в дату
x <- paste(au_wine$year_, au_wine$month_, sep="-")
date <- as.Date(paste(x,"-01",sep=""))

## 1. Есть ли у ряда тренд?

plot(date, au_wine$rose, type="l", ylab="rose")

# * Можем ли мы считать тренд линейным? Нелинейным?

# Тренд линейный нисходящий.
  
## 2. Есть ли у ряда сезонность?

# * Если сезонность есть, то к какому виду она относится: аддитивному или мультипликативному?

# Есть мультиплкативная сезонность

plot(date, log(au_wine$rose), type="l")

## Меняет ли ряд свой характер?
  
# Нет  

## 4. Есть ли в данных выбросы?

# Нет

## 5. Добавляем сезонные индикаторы

#  Время
time <- seq(nrow(au_wine))
#  Сезонные индикаторы
month <- as.factor(au_wine$month_)
# Объединяем результаты в таблицу
ts <- data.frame(rose=au_wine$rose, time, month)
# Убеждаемся, что сезонные индикаторы заданы фактором.
# Иначе не избежать ловушки индикаторных переменных
class(ts$month)

## 6. Строим линейную регрессионную модель

lm.01 <- lm(log(rose) ~ ., data = ts)
summary(lm.01)

plot(log(ts$rose), type="l")
lines(lm.01$fitted.values, col="red")

## 6. Прогнозируем логарифм ряда

# Создаем таблицу для новых значений
nrow.new <- nrow(ts)+8
month <- as.factor(rep(1:12, ceiling(nrow.new / 12))[1:nrow.new])
ts.new <- data.frame(rose=numeric(length=nrow.new), 
                     time = seq(nrow.new), 
                     month)
# Делаем прогноз при помощи модели lm.01
pred.lm.01 <- predict.lm(lm.01, ts.new[175:182,])
# Объединяем подгонку и прогноз
pred.lg <- c(lm.01$fitted.values, pred.lm.01)
# Выводим на график
plot(pred.lg, type="l", col="red")
lines(log(rose$wine))

## 7. Прогноз оригинального ряда

# Потенцируем результат
pred <- exp(pred.lg)
# Выводим результат и прогноз
plot(pred, type="l", col="red")
lines(ts$rose)

## Оценим погрешность прогноза на "хвосте" временного ряда.

ts.train <- ts[1:(nrow(ts)-8),]
ts.test <- ts[(nrow(ts)-8+1):nrow(ts),]

lm.02 <- lm(log(rose) ~ ., data = ts.train)
summary(lm.02)

pred.lm.02 <- predict.lm(lm.02, ts.test)

pred <- exp(pred.lm.02)

(RMSE <- sqrt(sum((ts.test$rose - pred)^2)/nrow(ts.test)))
(MAE <- mean(abs(ts.test$rose - pred)))

# Много это или мало?

mean(ts.test$rose)
# [1] 46.125

# Погрешность составляет от среднего за тестовый период, %:
round(RMSE/mean(ts.test$rose)*100,0)
# [1] 22


#### Australian monthly electricity production

au <- read.csv("elec.csv", header = T)

## 1. Есть ли у ряда тренд?

plot(au$elec, type="l", ylab="elec")

# * Можем ли мы считать тренд линейным? Нелинейным?

# Тренд линейный восходящий.

## 2. Есть ли у ряда сезонность?

# * Если сезонность есть, то к какому виду она относится: аддитивному или мультипликативному?

# Есть аддитивная сезонность

plot(log(au$elec), type="l")

## Меняет ли ряд свой характер?

# Да  

plot(au$elec[399:nrow(au)], type="l", ylab="elec")

## 4. Есть ли в данных выбросы?

# Нет

# Рассмотрим две модели: 
# 1) аддитивная сезонность на укороченном ряде
# 2) аддитивная сезонность на логарифмированном полном ряде

short <- au$elec[399:nrow(au)]

## 5. Добавляем сезонные индикаторы

# Время
time <- seq(short)
# Сезонные индикаторы
month <- rep(1:12, ceiling(nrow(au)/12))[1:nrow(au)]
month <- as.factor(month)
# Выделяем фрагмент month
month.short <- month[399:nrow(au)]
# Объединяем результаты в таблицу
ts <- data.frame(elec=short, time, month=month.short)
# Убеждаемся, что сезонные индикаторы заданы фактором.
# Иначе не избежать ловушки индикаторных переменных
class(ts$month)

lm.01 <- lm(elec ~ ., data = ts)
summary(lm.01)

# Call:
#   lm(formula = elec ~ ., data = ts)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -601.95 -107.58    1.23  135.68  575.95 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 12255.33     102.55 119.510  < 2e-16 ***
#   time           16.99       1.13  15.035  < 2e-16 ***
#   month2       -649.83     129.38  -5.023 4.23e-06 ***
#   month3        196.40     124.75   1.574 0.120265    
# month4       -512.74     124.71  -4.111 0.000113 ***
#   month5        562.27     124.69   4.509 2.78e-05 ***
#   month6        884.85     124.67   7.097 1.17e-09 ***
#   month7       1508.43     124.67  12.100  < 2e-16 ***
#   month8       1318.29     124.67  10.574 9.23e-16 ***
#   month9        152.63     129.45   1.179 0.242663    
# month10       122.98     129.42   0.950 0.345517    
# month11      -228.52     129.39  -1.766 0.082080 .  
# month12      -162.84     129.38  -1.259 0.212658    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 224.1 on 65 degrees of freedom
# Multiple R-squared:  0.9347,	Adjusted R-squared:  0.9226 
# F-statistic: 77.49 on 12 and 65 DF,  p-value: < 2.2e-16

plot(ts$elec, type="l")
lines(lm.01$fitted.values, col="red")

mm <- model.matrix(~month)

tss <- data.frame(elec=short, time, month=mm[399:nrow(au),-1])

lm.02 <- lm(elec ~ ., data = tss)
summary(lm.02)

# Call:
#   lm(formula = elec ~ ., data = tss)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -601.95 -107.58    1.23  135.68  575.95 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   12255.33     102.55 119.510  < 2e-16 ***
#   time             16.99       1.13  15.035  < 2e-16 ***
#   month.month2   -649.83     129.38  -5.023 4.23e-06 ***
#   month.month3    196.40     124.75   1.574 0.120265    
# month.month4   -512.74     124.71  -4.111 0.000113 ***
#   month.month5    562.27     124.69   4.509 2.78e-05 ***
#   month.month6    884.85     124.67   7.097 1.17e-09 ***
#   month.month7   1508.43     124.67  12.100  < 2e-16 ***
#   month.month8   1318.29     124.67  10.574 9.23e-16 ***
#   month.month9    152.63     129.45   1.179 0.242663    
# month.month10   122.98     129.42   0.950 0.345517    
# month.month11  -228.52     129.39  -1.766 0.082080 .  
# month.month12  -162.84     129.38  -1.259 0.212658    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 224.1 on 65 degrees of freedom
# Multiple R-squared:  0.9347,	Adjusted R-squared:  0.9226 
# F-statistic: 77.49 on 12 and 65 DF,  p-value: < 2.2e-16

tss.1 <- tss[,-c(4,10,11,13)]

lm.03 <- lm(elec ~ ., data = tss.1)
summary(lm.03)

# Call:
#   lm(formula = elec ~ ., data = tss.1)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -667.12  -96.44   19.65  138.40  572.68 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   12326.76      61.97 198.905  < 2e-16 ***
#   time             16.86       1.18  14.280  < 2e-16 ***
#   month.month2   -715.53     104.57  -6.843 2.56e-09 ***
#   month.month4   -578.99      98.04  -5.906 1.19e-07 ***
#   month.month5    496.16      98.04   5.061 3.31e-06 ***
#   month.month6    818.87      98.05   8.352 4.55e-12 ***
#   month.month7   1442.59      98.08  14.709  < 2e-16 ***
#   month.month8   1252.59      98.12  12.766  < 2e-16 ***
#   month.month11  -294.63     104.49  -2.820  0.00627 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 234.3 on 69 degrees of freedom
# Multiple R-squared:  0.9242,	Adjusted R-squared:  0.9154 
# F-statistic: 105.1 on 8 and 69 DF,  p-value: < 2.2e-16

## Оценим погрешность прогноза на "хвосте" временного ряда.

ts.train <- ts[1:(nrow(ts)-8),]
ts.test <- ts[(nrow(ts)-8+1):nrow(ts),]

lm.011 <- lm(elec ~ ., data = ts.train)
summary(lm.011)

pred <- predict.lm(lm.011, ts.test)

(RMSE <- sqrt(sum((ts.test$elec - pred)^2)/nrow(ts.test)))
# [1] 238.4009
(MAE <- mean(abs(ts.test$elec - pred)))
# [1] 177.8561

tss.train <- tss.1[1:(nrow(tss.1)-8),]
tss.test <- tss.1[(nrow(tss.1)-8+1):nrow(tss.1),]

lm.031 <- lm(elec ~ ., data = tss.train)
summary(lm.031)

pred <- predict.lm(lm.031, tss.test)

(RMSE <- sqrt(sum((tss.test$elec - pred)^2)/nrow(tss.test)))
# [1] 252.5372
(MAE <- mean(abs(tss.test$elec - pred)))
# [1] 193.1467

# Погрешность составляет, %
round(RMSE/mean(tss.test$elec)*100,0)
# [1] 2

# Модель (полная и сокращенная) очень хороша, 
# другой вариант пробовать не будем.


#### Sales for a souvenir shop

au <- read.csv("daily-min-temperatures.csv", header = T, 
               stringsAsFactors = F)

## 1. Есть ли у ряда тренд?

plot(au$Temp, type="l")

# * Можем ли мы считать тренд линейным? Нелинейным?

# Похоже, ряд стационарный.

## 2. Есть ли у ряда сезонность?

# * Если сезонность есть, то к какому виду она относится: 
#   аддитивному или мультипликативному?

# Есть аддитивная сезонность

## Меняет ли ряд свой характер?

# Нет  

## 4. Есть ли в данных выбросы?

# Нет

## 5. Добавляем сезонные индикаторы

# Сезонные индикаторы
month <- format(as.Date(au$Date), format = "%m")
month <- as.factor(month)
# Объединяем результаты в таблицу
ts <- data.frame(Temp=au$Temp, time=1:nrow(au), month)

lm.01 <- lm(Temp ~ ., data = ts)
summary(lm.01)

# Call:
#   lm(formula = Temp ~ ., data = ts)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7.5207 -1.8956 -0.1021  1.7623 11.9008 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.478e+01  1.734e-01  85.245  < 2e-16 ***
#   time         1.500e-04  4.377e-05   3.426 0.000618 ***
#   month02      3.388e-01  2.282e-01   1.485 0.137660    
# month03     -4.737e-01  2.227e-01  -2.127 0.033488 *  
#   month04     -2.955e+00  2.246e-01 -13.160  < 2e-16 ***
#   month05     -5.182e+00  2.228e-01 -23.262  < 2e-16 ***
#   month06     -7.775e+00  2.246e-01 -34.608  < 2e-16 ***
#   month07     -8.365e+00  2.228e-01 -37.537  < 2e-16 ***
#   month08     -7.171e+00  2.229e-01 -32.171  < 2e-16 ***
#   month09     -6.090e+00  2.248e-01 -27.092  < 2e-16 ***
#   month10     -4.762e+00  2.230e-01 -21.352  < 2e-16 ***
#   month11     -2.596e+00  2.249e-01 -11.542  < 2e-16 ***
#   month12     -1.228e+00  2.235e-01  -5.495 4.18e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.773 on 3637 degrees of freedom
# Multiple R-squared:  0.5379,	Adjusted R-squared:  0.5363 
# F-statistic: 352.8 on 12 and 3637 DF,  p-value: < 2.2e-16

plot(ts$Temp, type="l")
lines(lm.01$fitted.values, col="red")

# Мы пренебрегли суточными колебаниями температуры

## Оценим погрешность прогноза на "хвосте" временного ряда.

ts.train <- ts[1:(nrow(ts)-8*31),]
ts.test <- ts[(nrow(ts)-8*31+1):nrow(ts),]

lm.011 <- lm(Temp ~ ., data = ts.train)
summary(lm.011)

pred <- predict.lm(lm.011, ts.test)

(RMSE <- sqrt(sum((ts.test$Temp - pred)^2)/nrow(ts.test)))
# [1] 2.587879
(MAE <- mean(abs(ts.test$Temp - pred)))
# [1] 1.979212

plot(ts.test$Temp, type = "l")
lines(pred, col = "red")
