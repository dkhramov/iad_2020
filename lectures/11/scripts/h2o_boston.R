# Regression Example: Boston Housing
# Jo-fai (Joe) Chow - joe@h2o.ai
# H2O + LIME Workshop at eRum 2018 (Updated for MilanoR Workshop)
# https://nbviewer.jupyter.org/github/woobe/eRum_2018/blob/master/examples/regression_boston.html#automl-leaderboard

# install.packages("h2o", dependencies = T)
library(h2o)
library(mlbench) # наборы данных

## Подготовка к работе

# Зерно ГСЧ
seed <- 12345

# Загрузка данных
data(BostonHousing)

# Предикторы и отклик
target <- "medv" # медианная стоимость дома, 1000 USD
features <- setdiff(colnames(BostonHousing), target)

# Start a local H2O cluster (JVM)
h2o.init()
# Удаляем результаты предыдущего сеанса h2o
h2o.removeAll()

# Таблица R -> Таблица H2O
bh <- as.h2o(BostonHousing)
head(bh)

## Моделирование

# Разделим данные на обучающую и тестовую выборки
h_split <- h2o.splitFrame(bh, ratios = 0.75, seed = seed)
train <- h_split[[1]] # 75% на обучение
test  <- h_split[[2]] # 25% на проверку

# Обучим модель H2O glm с параметрами по умолчанию
model_glm <- h2o.glm(x = features,
                     y = target,
                     training_frame = train,
                     model_id = "glm_default_reg",
                     seed = seed)
print(model_glm)

# Оценка на тестовых данных
h2o.performance(model_glm, newdata = test)

# Обучим множество H2O-моделей с помощью AutoML
# Stacked Ensembles will be created from those H2O models
# You tell H2O ...
#     1) how much time you have and/or 
#     2) how many models do you want
# Note: H2O deep learning algo on multi-core is stochastic
model_automl <- h2o.automl(x = features,
                           y = target,
                           training_frame = train,
                           nfolds = 10,             # Cross-Validation
                           max_runtime_secs = 120,   # Max time
                           max_models = 100,         # Max no. of models
                           stopping_metric = "RMSE", # Metric to optimize
                           project_name = "automl_reg",
                           exclude_algos = NULL,     # If you want to exclude any algo 
                           seed = seed)
# Модели-лидеры
model_automl@leaderboard

# Лучшая модель
model_automl@leader

# Сравним одиночную базовую модель
h2o.performance(model_glm, newdata = test)
# с лучшей моделью, подобранной AutoML
h2o.performance(model_automl@leader, newdata = test)

# Прогноз
yhat_test <- h2o.predict(model_automl@leader, test)
head(yhat_test)
head(test$medv)

# Export Models
# h2o.saveModel() to save model to disk
# h2o.loadModel() to re-load model

## Интерпретация модели

# install.packages("lime", dependencies = T)
library(lime)

# Создадам эксплейнер
explainer <- lime(x = as.data.frame(train[, features]), 
                  model = model_automl@leader)

# Extract one sample (change `1` to any row you want)
samp <- as.data.frame(test[1, features])

# Assign a specifc row name (for better visualization)
row.names(samp) <- "Sample 1" 

# lime создает объяснения
explanations <- explain(x = samp,
                        explainer = explainer,
                        n_permutations = 5000,
                        feature_select = "auto",
                        n_features = 13) # Look top n features

# Смотрим объяснения
plot_features(explanations)

print(model_glm)
