# Загрузка данных
x.0 <- read.table("data/diamond.dat")
# Проверка
summary(x.0)

# Переставим столбцы и добавим столбец квадратов
x.1 <- cbind(x.0[,c(2,1)], x.0[,1]^2)
names(x.1) <- c("PRICE", "WEIGHT", "WEIGHT2")

# Простейшая модель
itog1 <- lm(PRICE ~ WEIGHT, x.1)
summary(itog1)

# Сделаем так как предлагают ювелиры
itog2 <- lm(PRICE ~ WEIGHT2, x.1)
summary(itog2)

# Модель, зависящая ото всех переменных
itog3 <- lm(PRICE ~ WEIGHT + WEIGHT2, x.1)
summary(itog3)

# Проверка коллинеарности WEIGHT и WEIGHT2

attach(x.1)

plot(WEIGHT, WEIGHT2)

cor(WEIGHT, WEIGHT2)
