library(ggplot2)


str(iris)

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point()


## Расстояние между кластерами: complete linkage

clusters <- hclust(dist(iris[, 3:4]))
plot(clusters, xlab=NA, sub=NA, labels=FALSE)

# Кластеров может быть 3 или 4
# Выберем вариант с 3-мя кластерами
cut <- cutree(clusters, 3)

table(cut, iris$Species)
# cut setosa versicolor virginica
# 1     50          0         0
# 2      0         21        50
# 3      0         29         0

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cut)


## Расстояние между кластерами: average linkage

clusters <- hclust(dist(iris[, 3:4]), method = 'average')

plot(clusters, xlab=NA, sub=NA, labels=FALSE)

# Кластеров может быть 3 или 5
# Выберем вариант с 3-мя кластерами
cut <- cutree(clusters, 3)

table(cut, iris$Species)
# cut setosa versicolor virginica
# 1     50          0         0
# 2      0         45         1
# 3      0          5        49

# Гораздо лучше. Ошибка всего в 6 наблюдениях

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = cut)


## Расстояние между кластерами: Ward

clusters <- hclust(dist(iris[, 3:4]), method = 'ward.D2')

plot(clusters, xlab=NA, sub=NA, labels=FALSE)

# Основной вариант - 3 кластера
cut <- cutree(clusters, 3)

table(cut, iris$Species)
# cut setosa versicolor virginica
# 1     50          0         0
# 2      0         45         1
# 3      0          5        49
