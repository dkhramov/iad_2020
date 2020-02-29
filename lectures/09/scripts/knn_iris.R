# 1. Загрузка и знакомство с данными

data(iris)
iris[1:5,]
summary(iris)
# Проверка сбалансированности классов
# table(iris$Species)

plot(iris$Petal.Length, iris$Sepal.Length, col=iris$Species)


# 2. Предварительная обработка

iris.new <- iris

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}

iris.new[,-5] <- as.data.frame(lapply(iris[,-5],normalize))

summary(iris.new)


# 3. Создание обучающей и тестовой выборок

ntrain <- round(0.8*nrow(iris.new), 0)

set.seed(123)
train_ind <- sample(1:nrow(iris), size = ntrain)

train <- iris.new[train_ind,-5]
train.target <- iris.new[train_ind,5]
test <- iris.new[-train_ind,-5]
test.target <- iris.new[-train_ind,5]


# 4. Применяем kNN

library(class)

model5 <- knn(train, test, cl=train.target, k=5)


# 5. Оценка точности прогноза

table(test.target, model5)
#              model5
# test.target  setosa versicolor virginica
# setosa          8          0         0
# versicolor      0          9         0
# virginica       0          1        12
