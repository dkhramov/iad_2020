library(caret)

# Загрузка данных
x <- read.table("../../data/Albuquerque_Home_Prices_data.txt", 
                   header=T, na.strings="-9999")
# Проверка
summary(x)
# Удаляем строки с NA
x <- na.omit(x)

set.seed(123)
ind_train <- createDataPartition(x$PRICE, p = 0.9, list = FALSE)
train <- x[ind_train,]
test <- x[-ind_train,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

rpart_grid <- expand.grid(cp=seq(0, 0.05, 0.005))

cart_fit <- train(PRICE ~., data = train, method = 'rpart',
                  trControl = ctrl,
                  metric='RMSE',
                  tuneGrid = rpart_grid)

plot(cart_fit)

plot(varImp(cart_fit))

prediction <- predict(cart_fit, newdata = test)

# Проверим точность прогноза
postResample(prediction, test$PRICE)

## А темперь вернемся к линейной регрессии

# Модель с удаленными коллинеарными переменными
# formula <- PRICE ~ SQFT + AGE + FEATS + NE + CUST + COR
formula <- PRICE ~ SQFT + AGE + CUST
lm_fit <- train(formula, 
                data = train, method = 'lm',
                trControl = ctrl,
                metric='RMSE')

prediction <- predict(lm_fit, newdata = test)

postResample(prediction, test$PRICE)
