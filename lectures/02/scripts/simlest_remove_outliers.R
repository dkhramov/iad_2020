# Наблюдение - количество изделий, произведенных фирмой за день.
production <- c(92, 100, 89, 98, 101, 84, 113, 93, 81, 14, 113, 86, 98, 99, 105, 88, 101, 89, 93, 102, 101, 99, 87, 109, 92, 99, 111, 98, 102, 95)

summary(production)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.00   89.75   98.00   94.40  101.00  113.00 

# Расхождение между средним и медианой велико. 
# Медиана ближе к 3-й квартили, чем к среднему.

hist(production)

(bp <- boxplot(production))
# List of 6
# $ stats: num [1:5, 1] 81 89 98 101 113
# $ n    : num 30
# $ conf : num [1:2, 1] 94.5 101.5
# $ out  : num 14
# $ group: num 1
# $ names: chr "1"

outliers <- boxplot(production, plot=FALSE)$out

production.1 <- production[!production %in% outliers]

summary(production.1)

hist(production.1)

# Теперь медиана почти такая же, как арифм. среднее. 
# Отличающееся данное почти не повлияло на медиану.
# По видимому, дневной нормой можно считать 97-98 шт. 
