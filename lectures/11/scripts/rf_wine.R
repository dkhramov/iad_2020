## Классификация вин при помощи случайного леса

library(randomForest)

setwd("week_13/data")
wine <- read.table("wine1.txt", header=T, sep="", dec=".")

# Формируем случайную подвыборку
set.seed(1234)
test.num <- sample(1:nrow(wine), 60, replace = FALSE)

# Тестовая выборка
test <- wine[test.num, -ncol(wine)]
# Код класса для тестовой выборки
y.test <- wine[test.num, ncol(wine)]

# Обучающая выборка
train <- wine[-test.num, ]
# Делаем колонку меток класса фактором
train$Wine_type <- as.factor(train$Wine_type)

# Предикторы
x <- train[, -ncol(wine)]

# Отклик
# Должен быть фактором, иначе выполняется регрессия
y <- train$Wine_type

# Значения результирующей переменной сбалансированы?
table(y)
# y
# 0  1  2 
# 40 32 46 

# При bagging'е используем датчик случайных чисел!
set.seed(3217)
# Число деревьев в лесе
ntree.1 <- 500 # так по умолчанию
ntree.1 <- 250 # то, что дал подбор числа деревьев
# Трассировка: число шагов через которые выдается 
# промежуточный результат
# do.trace = ntree.1/10
# ntree.1 <- 50   do.trace=ntree.1/25
# Минимальное число наблюдений в узле
nodesize.1 <- 1
# Сохранить обученный лес
keep.forest.1 <- TRUE

rf.wine <- randomForest(x, y, 
                       ntree = ntree.1, 
                       mtry = floor(sqrt(ncol(wine))),
                       replace = FALSE, 
                       nodesize = nodesize.1, 
                       importance = TRUE, 
                       do.trace = ntree.1/10, 
                       keep.forest = keep.forest.1)

# ntree      OOB      1      2      3
#    50:   1.69%  0.00%  0.00%  4.35%
#   100:   0.00%  0.00%  0.00%  0.00%
#   150:   0.00%  0.00%  0.00%  0.00%
#   200:   0.00%  0.00%  0.00%  0.00%
#   250:   0.00%  0.00%  0.00%  0.00%
#   300:   0.85%  2.50%  0.00%  0.00%
#   350:   0.85%  2.50%  0.00%  0.00%
#   400:   0.85%  2.50%  0.00%  0.00%
#   450:   0.85%  2.50%  0.00%  0.00%
#   500:   0.85%  2.50%  0.00%  0.00%

# Показывает
# какой процент ошибок допущен при распознавании 
# каждого из сортов вина.
# И самое важное: при каком количестве деревьев.
# (OOB - out-of bag error)
# Позволяет определить число деревьев.
# 
# Хороший результат получается уже при 100-250 деревьях

predict.wine <- predict(rf.wine, newdata = test)

# Оценим качество результата
table(y.test, predict.wine)
#       predict.wine
# y.test  0  1  2
#      0 19  0  0
#      1  0 16  0
#      2  1  2 22

# Как проходило голосование?
predict.wine.prob <- predict(rf.wine, newdata = test, 
                             type = "prob")

# График ошибки классификации
plot(randomForest(Wine_type ~ ., train, keep.forest=FALSE, 
                  ntree=500))

# График оценки влиятельности переменных
varImpPlot(rf.wine, sort=F)
# Извлекаем значения влиятельности в таблицу
import.wine <- importance(rf.wine, type=NULL, class=1, scale=TRUE)  

