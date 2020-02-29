library(caret)

data(iris)

set.seed(123)
ind_train <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
train <- iris[ ind_train,]
test  <- iris[-ind_train,]

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

knn_fit <- train(Species ~., data = train, method = 'knn',
                 trControl = ctrl,
                 preProcess = c('center', 'scale'),
                 tuneLength = 15)

prediction <- predict(knn_fit, newdata = test)

confusionMatrix(prediction, test$Species)
