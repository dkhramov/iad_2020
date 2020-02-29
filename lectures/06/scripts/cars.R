data("cars")

# Разделим данные на обучающую и тестовую выборки

# Зерно генератора случайных чисел нужно 
# для воспроизводимости результата
set.seed(100)
# Номера строк, которые попадут в обучающую выборку
train_ind <- sample(1:nrow(cars), 0.8*nrow(cars))
training <- cars[ train_ind, ] # обучающая выборка
test     <- cars[-train_ind, ] # тестовая выборка

# Построим модель, обученную на обучающей выборке
lm_mod <- lm(dist ~ speed, data=training)
# Спрогнозируем тормозной путь на тестовой выборке
predicted <- predict(lm_mod, test)

summary(lm_mod)

# Mean squared error:
MSE <- mean(lm_mod$residuals^2)
# Root MSE:
RMSE <- sqrt(MSE)

actuals_preds <- data.frame(actual=test$dist, predicted)
(correlation_accuracy <- cor(actuals_preds))
actuals_preds[1:5,]

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / 
                           apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicted -
                    actuals_preds$actual))/actuals_preds$actual)

min_max_accuracy
mape
