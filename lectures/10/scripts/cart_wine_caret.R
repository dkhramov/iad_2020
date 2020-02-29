library(caret)

wine <- read.table('../../data/wine.txt', header=T, sep="\t")

# Проверка: что находится в наборе?
wine[1:5,]
summary(wine)
dim(wine)

# Все переменные числовые и только тип вина Wine_type
# нужно переделать в фактор

wine$Wine_type <- as.factor(wine$Wine_type)

set.seed(123)
# Разделим данные на обучающую (70%) и тестовую (30%) выборки
ind_train <- createDataPartition(wine$Wine_type, p = 0.7, list = FALSE)
train <- wine[ind_train,]
test <- wine[-ind_train,]

# Проверим сбалансированность классов в оригинальных данных
# и обучающей выборке
prop.table(table(wine$Wine_type))*100
prop.table(table(train$Wine_type))*100

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

rpart_grid <- expand.grid(cp=seq(0, 0.05, 0.005))

# Используем information gain вместо gini для измерения загрзненности

cart_fit <- train(Wine_type ~., data = train, method = 'rpart',
                 trControl = ctrl,
                 #preProcess = c('center', 'scale'),
                 parms = list(split='information'),
                 tuneGrid = rpart_grid
                 )

plot(cart_fit)

plot(varImp(cart_fit))

prediction <- predict(cart_fit, newdata = test)

# Проверим точность прогноза
confusionMatrix(prediction, test$Wine_type)

# Построим дерево с библиотекой rattle
library(rattle)
fancyRpartPlot(cart_fit$finalModel)
