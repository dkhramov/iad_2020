# Читаем
setwd("week_07/data/")
df.0 <- read.table("Albuquerque_Home_Prices_data.txt", 
                   header=T, na.strings="-9999")
# Проверяем
summary(df.0)

# Чтобы не писать каждый раз "df.0"
attach(df.0)
# Вспоминаем данные
names(df.0)

# Рассмотрим зависимость налогов (TAX) от площади дома (SQFT)
plot(SQFT, TAX)

# Построим линейную регрессионную модель
reg <- lm(TAX ~ SQFT)
# Добавим к ней линию Y = a + bX
abline(a = reg$coefficients[1], b = reg$coefficients[2])
