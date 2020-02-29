# Читаем
setwd("week_06/data/")
df <- read.table("Albuquerque_Home_Prices_data.txt", header=T)
# Проверяем
summary(df)

# Обнаруживаем -9999 - это NA из пакета Statistica.

# Перечитываем с учетом этого.
df.0 <- read.table("Albuquerque_Home_Prices_data.txt", header=T, 
                   na.strings="-9999")
# Снова проверяем
summary(df.0)
