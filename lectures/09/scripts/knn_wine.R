# Чтение данных
setwd('../data')
wine <- read.table("wine.txt", header=T, sep="\t")

# Проверка: что находится в наборе?
wine[1:5,]
summary(wine)
dim(wine)

# Стандартизация
wine.st <- scale(wine[,-ncol(wine)], center = TRUE, scale = TRUE)

set.seed(1234)
# Формируем случайную подвыборку
test.num <- sample(1:nrow(wine.st), 60, replace = FALSE)

# Тестовая выборка
test <- wine.st[test.num, -ncol(wine)]

# Обучающая выборка
train <- wine.st[-test.num, -ncol(wine)]

# Код класса для обучающей выборки
cl <- wine[-test.num, ncol(wine)]

# Подключаем библиотеку class
library(class)

# Определим значение k
err <- rep(0,15)

for (i in 1:15)
{
  pred.knn <- knn(train, test, cl, k = i)
  err[i] <- sum(pred.knn != wine[test.num, ncol(wine)])
}

err

# Распознаем класс объектов из тестовой выборки
predicted.cl <- knn(train, test, cl, k = 1)

summary(predicted.cl)

# Проверяем соответствие распознанного класса истинному классу
table(predicted.cl, wine[test.num, ncol(wine)])
