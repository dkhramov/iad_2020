#### Пример классификации методом к-средних

#  Читаем данные примера
beverage.01 <- read.table("week_03/data/beverage.csv", header=T, sep=";")

# Вспомним имена переменных
names(beverage.01)

# Уберем ненужный 1-й столбец
beverage.01[,1] <- NULL

# Удачно угадаем, что кластеров три
summ.3 = kmeans(beverage.01, 3, iter.max = 100)
# Важная опция nstart - число стартовых попыток задать центры кластеров.
# Можете потом проделать те же вычисления, взяв 
# summ.3 = kmeans(beverage.01, 3, iter.max = 100, nstart = 10)

names(summ.3)
#  [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
#  [6] "betweenss"    "size"         "iter"         "ifault"  

# К каким кластерам принадлежат объекты?
summ.3$cluster
#   [1] 3 2 2 1 2 2 1 3 3 2 2 1 3 3 1 3 3 2 3 2 2 2 1 2 1 1 1 3 3 1 2 1 2 1
# У вас результат может отличаться.

#  Координаты центров кластеров
#  Основной источник вдохновения для интерпретации
summ.3$centers
#    COKE    D_COKE    D_PEPSI     D_7UP PEPSI    SPRITE       TAB   SEVENUP
#  1  0.0 1.0000000 0.54545455 0.5454545   0.0 0.0000000 0.9090909 0.0000000
#  2  1.0 0.2307692 0.07692308 0.0000000   1.0 0.1538462 0.0000000 0.2307692
#  3  0.7 0.3000000 0.10000000 0.1000000   0.3 0.9000000 0.1000000 0.6000000

#  Результат неудобно читать. 
options(digits=2)
summ.3$centers

# Транспонируем
t(summ.3$centers)
#                  1          2   3
#  COKE    0.0000000 1.00000000 0.7
#  D_COKE  1.0000000 0.23076923 0.3
#  D_PEPSI 0.5454545 0.07692308 0.1
#  D_7UP   0.5454545 0.00000000 0.1
#  PEPSI   0.0000000 1.00000000 0.3
#  SPRITE  0.0000000 0.15384615 0.9
#  TAB     0.9090909 0.00000000 0.1
#  SEVENUP 0.0000000 0.23076923 0.6

# Вернем отображение чисел как было
options(digits=7)

# Сумма квадратов расстояний от объектов кластера до центра кластера
summ.3$withinss
#  [1]  6.363636  7.230769 12.300000

# Сумма элементов предыдущего вектора
summ.3$tot.withinss

summ.3$totss
#  то же самое, что и 
#  sum(33*(apply(beverage.01, 2, sd))^2)

# Смотрим help
summ.3$tot.betweenss

# Размеры кластеров
summ.3$size
#  [1] 11 13 10


## Попробуем определить "правильное" число кластеров

# Максимальное число кластеров
n.clust <- 15
# Вектор для хранения результатов
wcss <- vector(mode="numeric", length=n.clust)
# Запускаем kmeans для k от 1 до 15
for(i in 1:n.clust){
  wcss[i] <- kmeans(beverage.01, centers=i)$tot.withinss
}
plot(1:n.clust, wcss, type="b", xlab="Number of Clusters",
     ylab="Within clusters sum of squares")


##  Попробуем решение с 4 кластерами и сравним результаты

summ.4 = kmeans(beverage.01, 4, iter.max = 100)

#  Для сравнения использовать команду
table(summ.3$cluster, summ.4$cluster)


##  Проекция данных на плоскость (Multidimentional scaling)

beverage.dist <- dist(beverage.01)
beverage.mds <- cmdscale(beverage.dist)

plot(beverage.mds, col = summ.3$cluster, xlab = "Index", ylab = "Y")
