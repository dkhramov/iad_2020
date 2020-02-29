# Шаг 0. Прочитаем данные
credit.01 <- read.table("week_12/data/credit.csv", header=T, sep=";")
# Проверка: импортировали правильно?
credit.01[1:5,]
dim(credit.01)
#  [1] 323   5

# К каким классам относятся переменные?
class(credit.01[ , 1])
class(credit.01[ , 2])
class(credit.01[ , 3])
class(credit.01[ , 4])
class(credit.01[ , 5])
#  [1] "integer"

# Теперь надо принимать решение: 
# должны быть переменные факторами или нет?
# То есть определяемся со шкалой, в которой измерены переменные.

#### Вариант 1.  Все переменные измерены в интервальной шкале

library(rpart)
credit.01.res <- rpart(кредит ~ ., 
                             data = credit.01, method="class",
                             control=rpart.control(minsplit=10, 
                                                   minbucket=5, 
                                                   maxdepth=6)
                      ) 

# credit.01.res - список с результатами (модель)
# rpart - функция, которая строит дерево классификации
# data = credit.01 - анализируются переменные из таблицы данных credit.01
# method="class" - строится дерево классификации, а не дерево регрессии
# model = TRUE - сохранить копию таблицы данных внутри модели (списка credit.01.res)
# control = rpart.control(minsplit=10, minbucket=5, maxdepth=6) ) - 
# какие правила останова использовать

## Деревья классификации: графика

# 1 вариант: некрасивое дерево
plot(credit.01.res)
text(credit.01.res, use.n=T)

# 2 вариант: красивое дерево, библиотека rpart.plot
library(rpart.plot)
# Базовый вариант: красиво, но неинформативно
rpart.plot(credit.01.res)
# Посмотрим другие варианты
rpart.plot(credit.01.res, type=0)
rpart.plot(credit.01.res, type=1)
rpart.plot(credit.01.res, type=2)
rpart.plot(credit.01.res, type=3)
rpart.plot(credit.01.res, type=4)
# Красиво, и информативно:
rpart.plot(credit.01.res, type=2, extra=1)

# 3 вариант: красивое дерево, библиотека rattle
library(rattle)
fancyRpartPlot(credit.01.res)

## Управление печатью результатов

print(credit.01.res, digits = 2)

## Подробный анализ результатов

summary(credit.01.res)

## Прогнозирование и подсчет ошибок

predicted <- predict(credit.01.res, credit.01[ ,-1], type="class")
table(predicted, credit.01[ ,1])

## Прогноз для новых данных

new <- data.frame(2,2,2,1) 
names(new) <- names(credit.01)[-1]

predicted.new <- predict(credit.01.res, new, type="class")

#### Вариант 2. Все переменные измерены в номинальной шкале.

# Преобразуем данные
credit.02 <- as.data.frame(lapply(credit.01, as.factor))
# Проверяем результат
str(credit.02)

# Зададим уровни факторов
levels(credit.02[,1]) <- c("Низкий", "Высокий")
levels(credit.02[,2]) <- c("Management", "Professional", "Clerical", 
                           "Skilled Manual", "Unskilled")
levels(credit.02[,3]) <- c("Еженедельно", "Ежемесячно")
levels(credit.02[,4]) <- c("Молод (< 25)", "Средний(25-35)", 
                           "Пожилой( > 35)")
levels(credit.02[,5]) <- c("Нет", "Да")

credit.02.res <- rpart(кредит ~ ., data = credit.02, 
                       method = "class", 
                       control = rpart.control(minsplit=10, 
                                               minbucket=5, 
                                               maxdepth=6)
                       ) 

rpart.plot(credit.02.res, type=2, extra=1)

predicted <- predict(credit.02.res, credit.02[ ,-1], 
                     type="class")
table(predicted, credit.02[ ,1])

#### Вариант 3. Все переменные измерены в порядковой шкале.

credit.03 <- data.frame(factor(credit.01[,1], ordered=T),
                        factor(credit.01[,2], levels = seq(5,1,-1), 
                               ordered = TRUE),
                        factor(credit.01[,3], ordered=T),
                        factor(credit.01[,4], ordered=T),
                        factor(credit.01[,5], ordered=T))

names(credit.03) <- names(credit.01)

str(credit.03)

# Зададим уровни факторов
levels(credit.03[,1]) <- c("Низкий", "Высокий")
levels(credit.03[,2]) <- c("Management", "Professional", "Clerical", 
                           "Skilled Manual", "Unskilled")
levels(credit.03[,3]) <- c("Еженедельно", "Ежемесячно")
levels(credit.03[,4]) <- c("Молод (< 25)", "Средний(25-35)", 
                           "Пожилой( > 35)")
levels(credit.03[,5]) <- c("Нет", "Да")

credit.03.res <- rpart(кредит ~ ., data = credit.03, 
                             method = "class", 
                             control = rpart.control(minsplit=10, 
                                                     minbucket=5, 
                                                     maxdepth=6)
) 

rpart.plot(credit.03.res, type=2, extra=1)

predicted <- predict(credit.03.res, credit.03[ ,-1], 
                     type="class")
table(predicted, credit.03[ ,1])
