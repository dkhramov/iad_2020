#### Выбор наилучшего числа кластеров в пакете NbClust
#### Версия 27.02.2017

##  Создаем набор данных, который предстоит кластеризовать

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

## Опеределяем число кластеров

# install.packages("NbClust")

library(NbClust)

Best <- NbClust(data.0, distance = "euclidean", 
                min.nc = 2, max.nc = 15, 
                method = "kmeans", # метод кластеризации
                index = "alllong"  # все способы
                )


