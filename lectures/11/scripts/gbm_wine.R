## Классификация вин при помощи градиентного бустинга

library(gbm)

# setwd("week_13/data")
wine <- read.table("wine1.txt", header=T, sep="", dec=".")

## По хорошему, здесь нужно выполнить отбор переменных

# Формируем случайную подвыборку
set.seed(1234)
test.num <- sample(1:nrow(wine), 60, replace = FALSE)

# Тестовая выборка
test <- wine[test.num, -ncol(wine)]
# Код класса для тестовой выборки
y.test <- wine[test.num, ncol(wine)]

# Обучающая выборка
train <- wine[-test.num, ]

# Соразмерно ли количество наблюдений разных классов?
table(wine$Wine_type)

set.seed(3217)
# Число деревьев в gbm
ntree.1 <- 500
# Минимальное число наблюдений в узле
nodesize.1 <-10

gbm.wine <- gbm(Wine_type~. , data=wine, 
               distribution="gaussian",     # "laplace"
               n.trees=ntree.1,             # 500
               shrinkage=0.05,              # 0.01 
               interaction.depth=5,         # 3-7
               bag.fraction = 0.66,         # 0.5 
               n.minobsinnode = nodesize.1, # 1
               cv.folds = 0,                # 0
               keep.data=TRUE,              # но не в production
               verbose=TRUE)                # вывод промежут. рез-тов

wine.predict <- predict(gbm.wine, newdata = test, n.trees = ntree.1)

# Вводим пороговые значения 0.5 и 1.5 и перекодируем
# вектор результатов регрессии wine.predict в вектор
# результатов классификации wine.cl
wine.cl <- rep(0, length(wine.predict))
wine.cl[(wine.predict > 0.5) & (wine.predict < 1.5)] <- 1
wine.cl[wine.predict > 1.5] <- 2

# Оцениваем качество
table(y.test, wine.cl)
#       wine.cl
# y.test  0  1  2
#      0 19  0  0
#      1  0 16  0
#      2  0  0 25
