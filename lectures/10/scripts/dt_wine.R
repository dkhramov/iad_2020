# Чтение данных
setwd('week_12/data')
wine <- read.table("wine.txt", header=T, sep="\t")

# Проверка: что находится в наборе?
wine[1:5,]
summary(wine)
dim(wine)

# Формируем случайную подвыборку
set.seed(1234)
test.num <- sample(1:nrow(wine), 60, replace = FALSE)

# Тестовая выборка
test <- wine[test.num, -ncol(wine)]

# Обучающая выборка
train <- wine[-test.num, ]

# Код класса для тестовой выборки
cl <- wine[test.num, ncol(wine)]

# Подключаем rpart
library(rpart)
wine.res <- rpart(Desired1.3. ~ ., 
                 data = train, method="class",
                 control=rpart.control(minsplit=10, 
                                       minbucket=5, 
                                       maxdepth=6)
                 )

library(rpart.plot)
rpart.plot(wine.res, type=2, extra=1)

## Прогнозирование и подсчет ошибок
predicted <- predict(wine.res, test, type="class")
table(predicted, cl)

