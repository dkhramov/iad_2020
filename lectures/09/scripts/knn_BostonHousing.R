library(caret)
library(mlbench)

# Загрузим данные
data(BostonHousing)

BostonHousing$chas <- as.numeric(as.character(BostonHousing$chas))
x <- as.matrix(BostonHousing[,1:13])
y <- as.matrix(BostonHousing[,14])

# Обучим модель
fit <- knnreg(x, y, k=3)

# Сделаем прогноз
predictions <- predict(fit, x)

# Оценим точность
(rmse <- sqrt(mean((BostonHousing$medv - predictions)^2)))