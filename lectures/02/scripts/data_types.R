## Типы данных

# Интервальные (количественные)

# создаем вектор
height <- c(174, 162, 188, 192, 165, 168, 174)
# смотрим структуру переменной
str(height)
# проверка: это вектор?
is.vector(height)
# проверка: он числовой?
is.numeric(height)
# строим график
plot(height)

# выбираем 1-й элемент
height[1]
# длина вектора
length(height)
# элементы со 2-го по 5-й
height[2:5]
# все элементы, кроме 1-го
height[-1]

# Номинативные

sex <- c("male", "female", "male", "male", "female", "female", "male")
str(sex)
is.character(sex)
plot(sex)

sex.f <- factor(sex)
plot(sex.f)
str(sex.f)

as.numeric(sex.f)

weight <- c(69, 68, 93, 87, 59, 82, 72)
plot(height, weight)
plot(height, weight, pch=as.numeric(sex.f))
plot(height, weight, pch=as.numeric(sex.f), col=as.numeric(sex.f))

levels(sex.f)
nlevels(sex.f)
nl <- nlevels(sex.f)
legend("topleft", pch=1:nl, col=1:nl, legend=levels(sex.f))

# шкальные

size <- c("L", "S", "XL", "XXL", "S", "M", "L")
size_f <- factor(size)
str(size_f)

size_o <- ordered(size_f, levels=c("S", "M", "L", "XL", "XXL"))
str(size_o)

## Списки и таблицы

l <- list(1,"a", c(TRUE, F))
str(l)
l <- list(num=1, ch="a", log=c(TRUE, F))
str(l)
l[1]
str(l[1])
l[[1]]
str(l[[1]])
l$log
length(l$log)

df <- data.frame(height, weight, sex, size)
df[1,1]
df$height[1]
df[,1]
df[,"height"]
