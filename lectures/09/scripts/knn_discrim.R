# Метод к-го ближайшего соседа (knn)
# Кросс-валидация методом тестового множества

# Чтение данных
setwd('../data')
sales <- read.table("discrim.txt", header=T, sep=";", row.names=1)

# Проверка
# Что находится в наборе?
sales[1:5,] # вместо head(sales)
summary(sales)
dim(sales)

# Чтобы результаты у меня и у вас совпадали
set.seed(1234)
# Формируем случайную подвыборку из 150*1/3 = 50 чисел
test.num <- sample(1:nrow(sales), 50, replace = FALSE)

# Тестовая выборка
test <- sales[test.num, 1:4]

# Обучающая выборка
train <- sales[-test.num, 1:4]

# Код класса для обучающей выборки
cl <- sales[-test.num, 5]

# Подключаем библиотеку class
library(class)

# Распознаем класс объектов из тестовой выборки
predicted.cl <- knn(train, test, cl, k = 3)

# Проверяем соответствие распознанного класса истинному классу
table(predicted.cl, sales[test.num, 5])

## Как определить значение k?
#  Используем простейший ваиант кросс-валидации

err <- rep(0,15)

for (i in 1:15)
{
  pred.knn <- knn(train, test, cl, k = i)
  err[i] <- sum(pred.knn != sales[test.num, 5])
}

err

predicted.cl <- knn(train, test, cl, k = 5)
table(predicted.cl, sales[test.num, 5])
