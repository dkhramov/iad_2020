library(mice) # md.pattern
library(DMwR) # regr.eval
library(VIM)  # kNN


data("BostonHousing", package="mlbench")

# Сохраним оригинальные данные
original <- BostonHousing
# Переобозначим датасет
bh <- BostonHousing
bh$rad <- as.factor(bh$rad)

# Добавим пропущенные значения
set.seed(100)
bh[sample(1:nrow(bh), 40), "rad"] <- NA
bh[sample(1:nrow(bh), 40), "ptratio"] <- NA

# Где в наборе данных находятся пропуски
md.pattern(bh)

kNN_out <- kNN(bh, variable = c("rad","ptratio"))
str(kNN_out)
# Появились новые переменные "rad_imp" и "ptratio_imp", 
# отмечающие место вставки

# Переменная rad: заполненные NA
kNN_out$rad[kNN_out$rad_imp]

# Проверим качество заполнения пропусков в ptratio
actuals <- original$ptratio[is.na(bh$ptratio)]
predicteds <- kNN_out[is.na(bh$ptratio), "ptratio"]
# Регрессия заполненных пропусков от истинных значений
regr.eval(actuals, predicteds)

# rad
actuals <- original$rad[is.na(bh$rad)]
predicteds <- kNN_out[is.na(bh$ptratio), "rad"]
regr.eval(actuals, predicteds)

