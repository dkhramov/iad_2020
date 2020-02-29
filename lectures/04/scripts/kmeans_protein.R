####  Пример классификации с помошью кластерного анализа
####  Версия 28.02.2017

## Шаг 1. Чтение данных

setwd("week_04/data")
data.01 <- read.table("Protein Consumption in Europe.csv", header=T, 
                       sep=";", dec = ",", row.names = 1)
# Проверим структуру таблицы: не получились ли строки вместо цифр?
str(data.01)

## Шаг 2. Удаление пропущенных значений

summary(data.01)

# В данной задаче пропущенных значений нет.

## Шаг 3. Стандартизация переменных.

# к среднему 0 и ст. отклонению 1
data.02 <- scale(data.01, center = TRUE, scale = TRUE)

## Шаг 4. Выбираем число кластеров k

# Максимальное число кластеров
n.clust <- 15
# Вектор для хранения результатов
wcss <- vector(length=n.clust)
for(i in 2:n.clust){
  wcss[i] <- kmeans(data.02, centers=i)$tot.withinss
}
plot(1:n.clust, wcss, type="b", xlab="Number of Clusters",
     ylab="Within clusters sum of squares")
# Предположительно кластеров k=5

##  Шаг 5. Процедура кластерного анализа

# Проводим кластерный анализ с выбранным k
clust = kmeans(data.02, 5, iter.max = 100, nstart = 10)

##  Шаг 6. Интерпретируем результаты

options(digits=2)
t(clust$centers)

# Само по себе это мало что дает.
# Какие страны попали в какие кластеры?

# Какие страны попали в 1-й кластер?
row.names(data.01[clust$cluster==1,])
# Их диета
colMeans(data.01[clust$cluster==1,])

row.names(data.01[clust$cluster==2,])
colMeans(data.01[clust$cluster==2,])

row.names(data.01[clust$cluster==3,])
colMeans(data.01[clust$cluster==3,])

row.names(data.01[clust$cluster==4,])
colMeans(data.01[clust$cluster==4,])

row.names(data.01[clust$cluster==5,])
colMeans(data.01[clust$cluster==5,])

## Шаг 7. Многомерное шкалирование

data.dist <- dist(data.02)
# Попробуйте использовать нестандартизированные данные
# и увидите, что получится
mds <- cmdscale(data.dist)

plot(mds, col = clust$cluster, xlab = "Index", ylab = "Y")
# Рисуем метки-названия стран
text(mds, labels = rownames(data.02), pos = 1, cex = 0.7)

