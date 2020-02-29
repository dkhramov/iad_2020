#### Аппроксимация точек многочленом
#### Вариант 2: понижаем степень полинома

## Шаг 0. Создаем данные

x <- seq(0, 6, 0.5)

set.seed(1234)
y <- sin(x) + rnorm(length(x), 0, 0.8)

plot(x, y, type="p", col="red")

# степень полинома
polinom.power <- 7

# Создаем обучающую выборку
train <- data.frame(x0=rep(1,length(x)), 
                    x1=x, x2=x^2, x3=x^3, x4=x^4, x5=x^5, 
                    x6=x^6, x7=x^7, y=y)

# Создаем тестовую выборку
x.test <- seq(min(x), max(x), 0.01)
test <- data.frame(x0=rep(1,length(x.test)), 
                   x1=x.test, x2=x.test^2, x3=x.test^3, x4=x.test^4, 
                   x5=x.test^5, x6=x.test^6, x7=x.test^7)

## Шаг 1. Используем процедуру lm

res.lm <- lm(y~.-x0, data=train)

y.pred <- predict.lm(res.lm, test)

plot(x.test, y.pred, type="l", col = "blue")
points(x, y, col="red")
# Переобучение

## Шаг 2. Получим beta прямыми вычислениями, не привлекая функцию lm

X <- as.matrix(train[ ,-ncol(train)])
y <- as.vector(train[ , ncol(train)])
beta <- solve(t(X) %*% X, t(X) %*% y)

# Матрица тестовых данных
X.test <- as.matrix(test)

# Прогноз на тестовых данных
y.pred.2 <- X.test %*% beta

# максимальное отличие между решениями, полученными на шагах 1 и 2
max(abs(y.pred.2 - y.pred))

## Шаг 3. С регуляризацией

lambda <- 1

# не учитываем столбец с y
num.parameters <- ncol(train)-1
# Создаем матрицу lambda*D
lambdaD <- lambda * diag(num.parameters)
lambdaD[1,1] <- 0

# Вычислим beta с регуляризованной матрицей
beta.r <- solve((t(X) %*% X + lambdaD), t(X) %*% y)

# Прогноз на тестовых данных
y.pred.3 <- X.test %*% beta.r

plot(x.test, y.pred.3, type="l", col = "blue")
points(x, y, col="red")



  
  
  
  