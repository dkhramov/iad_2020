# Все расчеты выполнены при одинаковых настройках классификтора!!!

#### Census_Income

library(rpart)
library(rpart.plot)

# Чтение данных

data <- readLines("adult.data")
data <- gsub(" ", "", data)
writeLines(data, "train.csv")
train <- read.table("train.csv", sep=",", na.strings = "?")
# Проверка: что находится в наборе?
dim(train)
summary(train)

data <- readLines("adult.test")
data <- gsub(" ", "", data)
writeLines(data, "test.csv")
test <- read.table("test.csv", sep=",", na.strings = "?")
# Проверка: что находится в наборе?
dim(test)
summary(test)

# Подключаем rpart
fit_income <- rpart(V15 ~ ., 
                    data = train, method="class",
                    control=rpart.control(minsplit = 10, 
                                          minbucket = 5, 
                                          maxdepth = 6)
)

rpart.plot(fit_income, type=2, extra=1)

# Прогнозирование и подсчет ошибок
pred <- predict(fit_income, test, type="class")
table(pred, test$V15)
# pred    <=50K. >50K.
# <=50K  11805  1901
# >50K     630  1945

# Общая точность
(11805+1945)/16281


#### Credit Approval

library(rpart)
library(rpart.plot)

# Чтение данных
crx <- read.table("crx.data", sep=",", na.strings = "?")

# Что находится в наборе?
dim(crx)
summary(crx)

# Формируем случайную подвыборку
set.seed(1234)
ind <- sample(1:nrow(crx), round(nrow(crx)*0.7,0))

# Обучающая выборка
train <- crx[ind, ]

# Тестовая выборка
test <- crx[-ind,]

# Подключаем rpart

fit_crx <- rpart(V16 ~ ., 
                 data = train, method="class",
                 control=rpart.control(minsplit=10, 
                                       minbucket=5, 
                                       maxdepth=6)
                 )


rpart.plot(fit_crx, type=2, extra=1)

## Прогнозирование и подсчет ошибок
pred <- predict(fit_crx, test, type="class")
table(pred, test$V16)
# pred   -   +
#   - 113  13
#   +  11  70

# Общая точность
(113+70)/207


#### Satellite

library(rpart)
library(rpart.plot)

# Чтение данных
train <- read.table("sat.trn", sep=" ", na.strings = "?")
# Что находится в наборе?
dim(train)
summary(train)

train$V37 <- as.factor(train$V37)

test <- read.table("sat.tst", sep=" ", na.strings = "?")
# Что находится в наборе?
dim(test)
summary(test)

test$V37 <- as.factor(test$V37)

# Подключаем rpart

fit_sat <- rpart(V37 ~ ., 
                 data = train, method="class",
                 control=rpart.control(minsplit=10, 
                                       minbucket=5, 
                                       maxdepth=6)
)


rpart.plot(fit_sat, type=2, extra=1)

## Прогнозирование и подсчет ошибок
pred <- predict(fit_sat, test, type="class")
table(pred, test$V37)
# pred   1   2   3   4   5   7
#    1 435  11   3  10  47  10
#    2   0 203   0   0   9   0
#    3   8   0 372  75   2  30
#    4   2   4  18  34   7  40
#    5   0   3   0   0 135   3
#    7  16   3   4  92  37 387

# Общая точность
(435+203+372+34+135+387)/2000
