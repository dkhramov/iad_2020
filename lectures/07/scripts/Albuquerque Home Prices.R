# Загрузка данных
setwd("week_07/data/")
x <- read.table("Albuquerque_Home_Prices_data.txt", 
                   header=T, na.strings="-9999")
# Проверка
summary(x)

# Построим модель, зависящую ото всех переменных
itog1 <- lm(PRICE ~ SQFT + AGE + FEATS + NE + CUST + COR + TAX, x)

summary(itog1)

# Еще один способ оценки коллинеарности
library(car)
vif(itog1)

# Оставим в модели только переменные с "ненулевыми" коэффициентами
itog2 <- lm(PRICE ~ SQFT + CUST + TAX, x)

summary(itog2)

# Проверим корреляцию между переменными
plot(x$SQFT, x$TAX)
cor(x$SQFT, x$TAX, use = "na.or.complete", method = "pearson")

# Еще интересные корреляции...

cor(x$SQFT, x$TAX, use = "complete.obs")

cor(x$SQFT, x$TAX, use = "complete.obs", method = "spearman")

cor(x, use = "complete.obs", method = "spearman")

cor(x, use = "complete.obs", method = "pearson")

pairs(~ SQFT + AGE + FEATS + TAX, data = x)

cor.test(x$SQFT, x$TAX, use = "complete.obs")


# Удалим налоги из модели
itog3 <- lm(PRICE ~ SQFT + AGE + FEATS + NE + CUST + COR, x)

summary(itog3)

# Еще одна попытка: по новым "ненулевым" коэффициентам
itog4 <- lm(PRICE ~ SQFT + AGE + CUST, x)

summary(itog4)
