# Читаем данные
setwd("week_05/data")
town.1959 <- read.table("town_1959.csv", header=TRUE, sep=",")
# Логарифмируем
log.town.1959 <- log(town.1959[,3])
# Тест Шапиро-Уилка
shapiro.test(log.town.1959)
