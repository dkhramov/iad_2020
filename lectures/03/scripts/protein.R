####  Пример классификации с помошью кластерного анализа
####  Версия 28.02.2017

##  Шаг 1. Чтение данных

setwd("week_03/data")
protein.01 <- read.table("Protein Consumption in Europe.csv", header=T, 
                         sep=";", dec = ",", row.names = 1)
# Проверим структуру таблицы: не получились ли строки вместо цифр?
str(protein.01)

##   Шаг 2. Удаление пропущенных значений

summary(protein.01)

# В данной задаче пропущенных значений нет.

##   Шаг 3. Стандартизация переменных.

# к среднему 0 и ст. отклонению 1
protein.02 <- scale(protein.01, center = TRUE, scale = TRUE)

##  Шаг 4. Процедура кластерного анализа

# Проводим кластерный анализ.
clust.protein <- hclust(dist(protein.02), "ward.D")

##  Шаг 5.  Построение дендрограммы

plot(clust.protein)

##  Шаг 6. Определение числа кластеров

# строим график "каменистая осыпь"
nclust <- seq(length(clust.protein$height),1,-1)
plot(nclust, clust.protein$height, type = "b",
     xlab = "Number of clusters", 
     ylab = "Height",
     main = "Scree plot")

# Ответ: 5 кластеров (а может быть 4)

# На дендрограмме красными прямоугольниками выделим 5 кластеров
rect.hclust(clust.protein, k=5, border="red") 

# Разделим наблюдения на 5 кластеров
# Вектор groups содержит номер кластера, в который попал 
# классифицируемый объект 
groups <- cutree(clust.protein, k=5) 
groups

## Шаг 7. Интерпретируем результаты кластеризации

# Какие страны попали в какие кластеры?
row.names(protein.01[groups==1,])
# Их диета
colMeans(protein.01[groups==1,])

row.names(protein.01[groups==2,])
colMeans(protein.01[groups==2,])

row.names(protein.01[groups==3,])
colMeans(protein.01[groups==3,])

row.names(protein.01[groups==4,])
colMeans(protein.01[groups==4,])

row.names(protein.01[groups==5,])
colMeans(protein.01[groups==5,])

