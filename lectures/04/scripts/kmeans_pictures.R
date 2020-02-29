#### Иллюстрация процесса кластеризации методом k-средних
#### Версия 27.02.2017


##  Шаг 1. Создаем набор данных, который предстоит кластеризовать

# Зададим зерно (начальное значение) для датчика случайных чисел
# чтобы у всех получались одинаковые картинки
set.seed(1234)

# Количество точек в каждом кластере
n.obj <- 900
# Стандартные отклонения (корень из дисперсии)
sd.1 <- 0.06

#  Генерируем кластеры

#  точки из 1-го кластера
x1 <- rnorm(n.obj, mean = 0.2, sd = sd.1)
y1 <- rnorm(n.obj, mean = 0.38, sd = sd.1)
#  точки из 2-го кластера
x2 <- rnorm(n.obj, mean = 0.49, sd = sd.1)
y2 <- rnorm(n.obj, mean = 0.25, sd = sd.1)
#  точки из 3-го кластера
x3 <- rnorm(n.obj, mean = 0.62, sd = sd.1)
y3 <- rnorm(n.obj, mean = 0.42, sd = sd.1)
#  точки из 4-го кластера
x4 <- rnorm(n.obj, mean = 0.42, sd = sd.1)
y4 <- rnorm(n.obj, mean = 0.78, sd = sd.1)
#  точки из 5-го кластера
x5 <- rnorm(n.obj, mean = 0.85, sd = sd.1)
y5 <- rnorm(n.obj, mean = 0.75, sd = sd.1)

# Объединяем данные в матрицу
x.0 <- c(x1, x2, x3, x4, x5)
y.0 <- c(y1, y2, y3, y4, y5)
data.0 <- cbind(x.0, y.0)


##  Шаг 2. Создаем начальные центры кластеров.
##         На практике - случайные. Сейчас берем такие,
##         чтобы процесс кластеризации выглядел выразительно

# Абсциссы точек
x.start <- c(0.50, 0.41, 0.43, 0.62, 0.38)
# Ординаты точек
y.start <- c(0.20, 0.22, 0.32, 0.36, 0.71)

# Объединяем данные в матрицу и помещаем ее в список
# для удобства работы с результатами кластеризации
clus.00 <- list()
clus.00$centers <- cbind(x.start, y.start)

#  Шаг 3. Проводим кластеризацию
#  Вместо выполнения 15 итераций разом,
#  15 раз выполняем по одной итерации.
#  Чтобы посмотреть, как работает процедура

clus.01 <- kmeans(data.0, centers=clus.00$centers, iter.max=1, algorithm = "Lloyd")
clus.02 <- kmeans(data.0, centers=clus.01$centers, iter.max=1, algorithm = "Lloyd")
clus.03 <- kmeans(data.0, centers=clus.02$centers, iter.max=1, algorithm = "Lloyd")
clus.04 <- kmeans(data.0, centers=clus.03$centers, iter.max=1, algorithm = "Lloyd")
clus.05 <- kmeans(data.0, centers=clus.04$centers, iter.max=1, algorithm = "Lloyd")
clus.06 <- kmeans(data.0, centers=clus.05$centers, iter.max=1, algorithm = "Lloyd")
clus.07 <- kmeans(data.0, centers=clus.06$centers, iter.max=1, algorithm = "Lloyd")
clus.08 <- kmeans(data.0, centers=clus.07$centers, iter.max=1, algorithm = "Lloyd")
clus.09 <- kmeans(data.0, centers=clus.08$centers, iter.max=1, algorithm = "Lloyd")
clus.10 <- kmeans(data.0, centers=clus.09$centers, iter.max=1, algorithm = "Lloyd")
clus.11 <- kmeans(data.0, centers=clus.10$centers, iter.max=1, algorithm = "Lloyd")
clus.12 <- kmeans(data.0, centers=clus.11$centers, iter.max=1, algorithm = "Lloyd")
clus.13 <- kmeans(data.0, centers=clus.12$centers, iter.max=1, algorithm = "Lloyd")
clus.14 <- kmeans(data.0, centers=clus.13$centers, iter.max=1, algorithm = "Lloyd")
clus.15 <- kmeans(data.0, centers=clus.14$centers, iter.max=1, algorithm = "Lloyd")


##  Шаг 4.  Строим графики процесса кластеризации

# Задаем размер точек на графиках, которые появятся позднее
cex.1 <- 0.2
# Задаем цвета кластеров, которые появятся позднее
col.1 <- c("green", "blue", "cyan", "darkorchid", "darkgoldenrod")

#  Итерация 0 - что имеем до начала применения процедуры

#  Итерация 1

# Исходные данные
plot(data.0, col="blue", pch=19, main="Iteration 0", cex=cex.1)
# Центры кластеров в начале итерации
points(clus.00$centers, col="red", pch=19, cex=9*cex.1)

# Распределяем объекты по кластерам
plot(data.0, col=col.1[clus.01$cluster], pch=19, main="Iteration 1", cex=cex.1)
# Исходные центры кластеров
points(clus.00$centers, col="red", pch=19, cex=9*cex.1)
# Новые центры кластеров
points(clus.01$centers, col="black", pch=19, cex = 9*cex.1)
# Стрелки - перемещение центров кластеров
arrows(clus.00$centers[,1], clus.00$centers[,2], 
       x1=clus.01$centers[,1], y1=clus.01$centers[,2], 
       col="red", lwd=2, angle=15, length=0.1)

# Итерация 2

# Исходные данные
plot(data.0, col="blue", pch=19, main="Iteration 2", cex=cex.1)
points(clus.01$centers, col="red", pch=19, cex=9*cex.1)
# Распределяем объекты по кластерам и отображаем перемещение центров
plot(data.0, col=col.1[clus.02$cluster], pch=19, main="Iteration 2", cex=cex.1)
points(clus.01$centers, col="red", pch=19, cex=9*cex.1)
points(clus.02$centers, col="black", pch=19, cex=9*cex.1)
arrows(clus.01$centers[,1], clus.01$centers[,2], 
       x1=clus.02$centers[,1], y1=clus.02$centers[,2], 
       col="red", lwd=2, angle=15, length=0.1)

# Итерация 3

# Исходные данные
plot(data.0, col="blue", pch=19, main="Iteration 3", cex=cex.1)
points(clus.02$centers, col="red", pch=19, cex=9*cex.1)
# Распределяем объекты по кластерам и отображаем перемещение центров
plot(data.0, col=col.1[clus.03$cluster], pch=19, main="Iteration 3", cex=cex.1)
points(clus.02$centers, col="red", pch=19, cex=9*cex.1)
points(clus.03$centers, col="black", pch=19, cex=9*cex.1)
arrows(clus.02$centers[,1], clus.02$centers[,2], 
       x1=clus.03$centers[,1], y1=clus.03$centers[,2], 
       col="red", lwd=2, angle=15, length=0.1)


##  Шаг 5. Как перераспределялись объекты между кластерами

plot(data.0, col="blue", pch=19, main="Iteration 0", cex=cex.1)
plot(data.0, col=col.1[clus.01$cluster], pch=19, main="Iteration 1", cex=cex.1)
plot(data.0, col=col.1[clus.02$cluster], pch=19, main="Iteration 2", cex=cex.1)
plot(data.0, col=col.1[clus.03$cluster], pch=19, main="Iteration 3", cex=cex.1)
plot(data.0, col=col.1[clus.04$cluster], pch=19, main="Iteration 4", cex=cex.1)
plot(data.0, col=col.1[clus.05$cluster], pch=19, main="Iteration 5", cex=cex.1)
plot(data.0, col=col.1[clus.06$cluster], pch=19, main="Iteration 6", cex=cex.1)
plot(data.0, col=col.1[clus.07$cluster], pch=19, main="Iteration 7", cex=cex.1)
plot(data.0, col=col.1[clus.08$cluster], pch=19, main="Iteration 8", cex=cex.1)
plot(data.0, col=col.1[clus.09$cluster], pch=19, main="Iteration 9", cex=cex.1)
plot(data.0, col=col.1[clus.10$cluster], pch=19, main="Iteration 10", cex=cex.1)
plot(data.0, col=col.1[clus.11$cluster], pch=19, main="Iteration 11", cex=cex.1)
plot(data.0, col=col.1[clus.12$cluster], pch=19, main="Iteration 12", cex=cex.1)
plot(data.0, col=col.1[clus.13$cluster], pch=19, main="Iteration 13", cex=cex.1)
plot(data.0, col=col.1[clus.14$cluster], pch=19, main="Iteration 14", cex=cex.1)
plot(data.0, col=col.1[clus.15$cluster], pch=19, main="Iteration 15", cex=cex.1)

# Сохранение графика в файл PNG
png(file = "iter0.png", bg = "transparent")
plot(data.0, col="blue", pch=19, main="Iteration 0", cex=cex.1)
dev.off()

## График каменистая осыпь

# Таблица для хранения результатов
cost_df <- data.frame()
# Запускаем kmeans для k от 1 до 15
for(k in 1:15){
  kmeans<- kmeans(x=data.0, centers=k, iter.max=100, algorithm = "Lloyd")
  # Соберем число кластеров и оценку внутрикластерного расстояния
  # и запишем в таблицу
  cost_df<- rbind(cost_df, cbind(k, kmeans$tot.withinss))
}
# Даем имена столбцам таблицы
names(cost_df) <- c("N.clusters", "W")
# Строим график
plot(cost_df, type="b")

