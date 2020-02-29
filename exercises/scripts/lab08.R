#### BreastCancer

library(class) # knn

# Метод к-го ближайшего соседа (knn)
# Кросс-валидация методом тестового множества

# Чтение данных
breast <- read.csv("breast-cancer-wisconsin.data")

# Что находится в наборе?
dim(breast)
summary(breast)
any(is.na(breast))

# Предобработка

# knn не работает с пропусками данных.
# Удаляем их

breast$X1.3[breast$X1.3 == "?"] <- NA
sum(is.na(breast))
# Преобразуем все в числа
breast <- breast[complete.cases(breast),]
breast[,-1] <- apply(breast[,-1], 2, as.integer)

# Удалим id анализа и метку класса
dat <- as.matrix(breast[,-c(1,11)])
# Изменим обозначение класса
# (2 - доброкачественная, 4 - злокачественная)
# на 0 - доброкачественная, 1 - злокачественная
cl <- breast[,11]
cl[cl == 2] <- 0
cl[cl == 4] <- 1
cl <- as.factor(cl)

set.seed(1234)
# Формируем случайную подвыборку из N*2/3 наблюдений
train.ind <- sample(1:nrow(dat), 
                    round(dim(dat)*2/3,0))

# Обучающая выборка
train <- dat[train.ind,]
# Тестовая выборка
test <- dat[-train.ind,]

# Код класса для обучающей и тестовой выборки
cl.train <- cl[train.ind]
cl.test  <- cl[-train.ind]

# Определим наилучшее значение k
# по минимуму ошибок на тестовой выборке

err <- rep(0,15)

for (i in 1:15) {
  pred <- knn(train, test, cl.train, k = i)
  err[i] <- sum(pred != cl.test)
}

# Итоги на графике
barplot(err, names.arg=1:15)
# k_best = 6

predicted <- knn(train, test, cl.train, k = 6)
table(predicted, cl.test)
#           cl.test
# predicted   0   1
#         0 138   4
#         1   5  80

# Общая точность
(138+80)/227


#### Sonar

library(class) # knn

# Чтение данных
sonar <- read.csv("sonar.csv")

# Что находится в наборе?
dim(sonar)
summary(sonar)
any(is.na(sonar))
# Пропусков данных нет

# Предобработка

# Удалим id анализа и метку класса
dat <- as.matrix(sonar[,-61])
cl <- sonar[,61]

set.seed(1234)
# Формируем случайную подвыборку из N*2/3 наблюдений
train.ind <- sample(1:nrow(dat), 
                    round(dim(dat)*2/3,0))

# Обучающая выборка
train <- dat[train.ind,]
# Тестовая выборка
test <- dat[-train.ind,]

# Код класса для обучающей и тестовой выборки
cl.train <- cl[train.ind]
cl.test  <- cl[-train.ind]

# Определим наилучшее значение k
# по минимуму ошибок на тестовой выборке

err <- rep(0,15)

for (i in 1:15) {
  pred <- knn(train, test, cl.train, k = i)
  err[i] <- sum(pred != cl.test)
}

# Итоги на графике
barplot(err, names.arg=1:15)
# Наилучшее значение k = 1, но велик риск зашумленности.
# Выберем k = 3 (для бин. классификации 
# желательно нечетное число соседей)

predicted <- knn(train, test, cl.train, k = 3)
table(predicted, cl.test)
#          cl.test
# predicted  M  R
#          M 30  5
#          R 11 23

# Общая точность
(30+23)/69


#### Glass

library(mlbench) # для набора данных
library(caret)

# Разведочный анализ данных

# Загрузим данные из набора Glass
data(Glass)

# К каким типам относятся переменные
str(Glass)
# 'data.frame':	213 obs. of  10 variables:
# $ RI  : num  1.52 1.52 1.52 1.52 1.52 ...
# $ Na  : num  13.6 13.9 13.5 13.2 13.3 ...
# $ Mg  : num  4.49 3.6 3.55 3.69 3.62 3.61 3.6 3.61 3.58 3.6 ...
# $ Al  : num  1.1 1.36 1.54 1.29 1.24 1.62 1.14 1.05 1.37 1.36 ...
# $ Si  : num  71.8 72.7 73 72.6 73.1 ...
# $ K   : num  0.06 0.48 0.39 0.57 0.55 0.64 0.58 0.57 0.56 0.57 ...
# $ Ca  : num  8.75 7.83 7.78 8.22 8.07 8.07 8.17 8.24 8.3 8.4 ...
# $ Ba  : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Fe  : num  0 0 0 0 0 0.26 0 0 0 0.11 ...
# $ Type: Factor w/ 6 levels "1","2","3","5",..: 1 1 1 1 1 1 1 1 1 1 ...

# Есть ли пропуски в данных?
anyNA(Glass)
# [1] FALSE

# Если ли повторяющиеся строки?
anyDuplicated(Glass)
# [1] 40

# Удалим повторяющиеся строки
Glass <- Glass[!duplicated(Glass),]

# Как распределяются наблюдения по классам?
table(Glass$Type)
#  1  2  3  5  6  7 
# 69 76 17 13  9 29
# Налицо дисбаланс классов

# Разделим данные на обучающую и тестовую выборки
set.seed(1234)
split_data <- createDataPartition(Glass$Type, p = 0.8, list = FALSE)
train <- Glass[split_data,]
test  <- Glass[-split_data,]

## Чистый knn

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

fit_knn <- train(Type ~., data = train, method = 'knn',
                 trControl = ctrl,
                 preProcess = c('center', 'scale'),
                 tuneLength = 15)

# Посмотрим, как подбиралось k
fit_knn

# Спрогнозируем
prediction <- predict(fit_knn, newdata = test)

# Проверим точность прогноза
confusionMatrix(prediction, test$Type)
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction  1  2  3  5  6  7
#           1 10  5  3  0  0  0
#           2  3  8  0  1  0  0
#           3  0  0  0  0  0  0
#           5  0  2  0  0  0  0
#           6  0  0  0  0  1  0
#           7  0  0  0  1  0  5
# 
# Overall Statistics
# 
# Accuracy : 0.6154          
# 95% CI : (0.4462, 0.7664)
# No Information Rate : 0.3846          
# P-Value [Acc > NIR] : 0.002946        
# 
# Kappa : 0.4543          
# 
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
# Class: 1 Class: 2 Class: 3 Class: 5 Class: 6 Class: 7
# Sensitivity            0.7692   0.5333  0.00000  0.00000  1.00000   1.0000
# Specificity            0.6923   0.8333  1.00000  0.94595  1.00000   0.9706
# Pos Pred Value         0.5556   0.6667      NaN  0.00000  1.00000   0.8333
# Neg Pred Value         0.8571   0.7407  0.92308  0.94595  1.00000   1.0000
# Prevalence             0.3333   0.3846  0.07692  0.05128  0.02564   0.1282
# Detection Rate         0.2564   0.2051  0.00000  0.00000  0.02564   0.1282
# Detection Prevalence   0.4615   0.3077  0.00000  0.05128  0.02564   0.1538
# Balanced Accuracy      0.7308   0.6833  0.50000  0.47297  1.00000   0.9853

### Пополнение выборки для исправления дисбаланса классов

Glass_up <- upSample(Glass[,-10], Glass$Type, yname="Type")

table(Glass_up$Type)
# 1  2  3  5  6  7 
# 76 76 76 76 76 76 

# Разделим данные на обучающую и тестовую выборки
set.seed(1234)
split_data <- createDataPartition(Glass_up$Type, p = 0.8, list = FALSE)
train <- Glass_up[split_data,]
test  <- Glass_up[-split_data,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

fit_knn_up <- train(Type ~., data = train, method = 'knn',
                 trControl = ctrl,
                 preProcess = c('center', 'scale'),
                 tuneLength = 15)

# Посмотрим, как подбиралось k
fit_knn_up

# Спрогнозируем
prediction <- predict(fit_knn_up, newdata = test)

# Проверим точность прогноза
confusionMatrix(prediction, test$Type)
# Confusion Matrix and Statistics
# 
#             Reference
# Prediction  1  2  3  5  6  7
#          1 10  7  0  0  0  1
#          2  3  6  0  2  0  0
#          3  2  2 15  0  0  0
#          5  0  0  0 13  0  1
#          6  0  0  0  0 15  0
#          7  0  0  0  0  0 13
# 
# Overall Statistics
# 
# Accuracy : 0.8             
# 95% CI : (0.7025, 0.8769)
# No Information Rate : 0.1667          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.76            
# 
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 5 Class: 6 Class: 7
# Sensitivity            0.6667  0.40000   1.0000   0.8667   1.0000   0.8667
# Specificity            0.8933  0.93333   0.9467   0.9867   1.0000   1.0000
# Pos Pred Value         0.5556  0.54545   0.7895   0.9286   1.0000   1.0000
# Neg Pred Value         0.9306  0.88608   1.0000   0.9737   1.0000   0.9740
# Prevalence             0.1667  0.16667   0.1667   0.1667   0.1667   0.1667
# Detection Rate         0.1111  0.06667   0.1667   0.1444   0.1667   0.1444
# Detection Prevalence   0.2000  0.12222   0.2111   0.1556   0.1667   0.1444
# Balanced Accuracy      0.7800  0.66667   0.9733   0.9267   1.0000   0.9333
