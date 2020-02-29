library(caret)

alb.0 <- read.table("data/Albuquerque_Home_Prices_data.txt", 
                   header=T, na.strings="-9999")
summary(alb.0)

##

# Данных мало, поэтому вместо удаления заполним пропуски методом kNN
# alb <- alb.0[complete.cases(alb.0),]
alb <- alb.0

(preProcess_missingdata_model <- preProcess(alb, method='knnImpute'))

library(RANN)  # нужна дляr knnInpute
alb <- predict(preProcess_missingdata_model, newdata = alb)

##

set.seed(1234)
tr_index <- createDataPartition(alb$PRICE, p = .75, list = FALSE)
tr <- alb[ tr_index, ]
te <- alb[-tr_index, ]

set.seed(5678)
lm_fit.1 <- train(PRICE ~ .,
                  data = tr, 
                  method = "lm")
summary(lm_fit.1)

alb_pred <- predict(lm_fit.1, te)
postResample(pred = alb_pred, obs = te$PRICE)

##

car::vif(lm_fit.1$finalModel)
findCorrelation(cor(alb))

# Удалим налоги из модели
set.seed(5678)
lm_fit.2 <- train(PRICE ~ . -TAX,
                  data = tr, 
                  method = "lm")
summary(lm_fit.2)

alb_pred <- predict(lm_fit.2, te)
postResample(pred = alb_pred, obs = te$PRICE)

##

set.seed(5678)
lm_fit.3 <- train(PRICE ~ SQFT + AGE + CUST,
                  data = tr, 
                  method = "lm")
summary(lm_fit.3)

alb_pred <- predict(lm_fit.3, te)
postResample(pred = alb_pred, obs = te$PRICE)

##

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

rf_fit.1 <- train(PRICE ~ .,
                  tr,
                  method = "ranger",
                  trControl = ctrl)
rf_fit.1

alb_pred <- predict(rf_fit.1, te)
postResample(pred = alb_pred, obs = te$PRICE)
