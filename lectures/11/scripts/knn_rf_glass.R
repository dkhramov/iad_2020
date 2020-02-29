library(mlbench)
library(caret)

# RF со сбалансированными классами

# Загрузим данные из набора Glass
data(Glass)

# К каким типам относятся переменные
str(Glass)

# Есть ли пропуски в данных?
anyNA(Glass)

# Если ли повторяющиеся строки?
anyDuplicated(Glass)

# Удалим повторяющиеся строки
Glass.1 <- Glass[!duplicated(Glass),]

# Как распределяются наблюдения по классам?
table(Glass.1$Type)
#  1  2  3  5  6  7 
# 69 76 17 13  9 29
# Налицо дисбаланс классов

# Разделим данные на обучающую и тестовую выборки
set.seed(1234)
split_data <- createDataPartition(Glass.1$Type, p = 0.7, list = FALSE)
train_data <- Glass.1[split_data,]
test_data <- Glass.1[-split_data,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

## Чистый knn

fit_knn <- train(Type ~., data = train_data, method = 'knn',
                 trControl = ctrl,
                 preProcess = c('center', 'scale'),
                 tuneLength = 15)

# Посмотрим, как подбиралось k
fit_knn

# Спрогнозируем
prediction <- predict(fit_knn, newdata = test_data)

# Проверим точность прогноза
confusionMatrix(prediction, test_data$Type)

# Overall Statistics
# 
# Accuracy : 0.7333          
# Kappa : 0.6206          
# 
# Statistics by Class:
# 
#                        Class: 1 Class: 2 Class: 3 Class: 5 Class: 6 Class: 7
# Sensitivity            0.7500   0.7727  0.00000  0.66667  1.00000   1.0000
# Specificity            0.8000   0.8158  1.00000  1.00000  1.00000   0.9808

## Чистый RF

fit_rf <- train(Type~.,
                 train_data,
                 method = "ranger",
                 preProc=c("center", "scale"),
                 trControl = ctrl)

fit_rf

pred_rf <- predict(fit_rf, test_data)                           
confusionMatrix(pred_rf, test_data$Type) 

# Overall Statistics
# 
# Accuracy : 0.8667          
# Kappa : 0.8111          
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3 Class: 5 Class: 6 Class: 7
# Sensitivity            0.8500   1.0000  0.20000  0.66667  1.00000   1.0000
# Specificity            0.9250   0.8947  1.00000  1.00000  1.00000   0.9808

# Без стандартизации результат получится хуже!

## knn со сбалансированными классами

# It is a very inbalanced dataset, 
# so we try all type of glass be enough represented in the training set
#https://topepo.github.io/caret/subsampling-for-class-imbalances.html

train_data <- upSample(train_data[,-10], train_data$Type, yname="Type")
table(train_data$Type)

fit_knn <- train(Type ~., data = train_data, method = 'knn',
                 trControl = ctrl,
                 preProcess = c('center', 'scale'),
                 tuneLength = 15)

# Посмотрим, как подбиралось k
fit_knn

# Спрогнозируем
prediction <- predict(fit_knn, newdata = test_data)

# Проверим точность прогноза
confusionMatrix(prediction, test_data$Type)

# Overall Statistics
# 
# Accuracy : 0.7             
# Kappa : 0.6108          
# 
# Statistics by Class:
# 
#                        Class: 1 Class: 2 Class: 3 Class: 5 Class: 6 Class: 7
# Sensitivity            0.5500   0.6818  1.00000  0.66667  1.00000   0.8750
# Specificity            0.9250   0.9211  0.83636  0.98246  0.98276   0.9808

## RF со сбалансированными классами

train_data <- upSample(train_data[,-10], train_data$Type, yname="Type")
table(train_data$Type)

fit_rf <- train (Type~.,
                 train_data,
                 method = "ranger",
                 preProc=c("center", "scale"),
                 trControl = ctrl)

fit_rf

pred_rf <- predict(fit_rf, test_data)                           
confusionMatrix(pred_rf, test_data$Type) 

# Overall Statistics
# 
# Accuracy : 0.8333          
# Kappa : 0.7678          
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 5 Class: 6 Class: 7
# Sensitivity            0.7000   1.0000  0.40000  0.66667  1.00000   1.0000
# Specificity            0.9500   0.8684  0.96364  1.00000  1.00000   0.9808

##

# Разделим данные на обучающую и тестовую выборки
set.seed(1234)
split_data <- createDataPartition(Glass.1$Type, p = 0.7, list = FALSE)
train_data <- Glass.1[split_data,]
test_data <- Glass.1[-split_data,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

model_grid <- expand.grid(.mtry = 2:5,
                          .splitrule = "gini",
                          .min.node.size = 5)

fit_rf <- train(Type~.,
                train_data,
                method = "ranger",
                tuneGrid = model_grid,
                preProc=c("center", "scale"),
                trControl = ctrl)

fit_rf

pred_rf <- predict(fit_rf, test_data)                           
confusionMatrix(pred_rf, test_data$Type) 
