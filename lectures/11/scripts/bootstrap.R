library(bootstrap)

data(iris)

set.seed(123) 

# Выборка длин чашелистиков
data <- iris$Sepal.Length

# Запустим бутстреп для sd 200 раз
bs <- bootstrap(data, nboot = 200, sd)

# Сохраним все полученные значения sd
values <- bs$thetastar

# Оценим среднее, sd и доверительный интервал
mean(values)
sd(values)
quantile(values,c(0.05,0.95))
