#### Предварительная обработка данных
#### По мотивам статьи "Как я победил в конкурсе BigData от Beeline"
#### https://habrahabr.ru/post/270367/

train <- read.table(unzip("../data/train.zip"), header = T, sep=",")

str(train)

#  x8
#  x23-x61


## Шаг 1. Предварительный просмотр факторов
# факторы - переменные в номинальной шкале

class(train[,4])

# Смотрим уровни фактора
levels(train[,4])

# перекодировка
z.tmp <- as.numeric(train[,4]) 

# Сколько всего уникальных значений?
length(unique(train[,4])) 

# Сколько раз встречается каждый из уровней?
table(train[,4])


## Шаг 2. Предварительный просмотр переменных, 
##        заданных в количественной шкале

# Как распределены числовые данные?
hist(train[,25])
# Не слишком ли мало столбцов?
hist(train[,25],breaks=100) 
# Осмотрим на предмет выбросов
boxplot(train[,25])

# Выполним логарифмическое преобразование

# Сохраним неизменным исходный столбец данных
zzz <- train[,25]
# Исключим отрицательные значения и ноль
zzz2 <- zzz - min(zzz, na.rm = T) + 0.01
# Посмотрим на результаты логарифмирования
hist(log(zzz2)) 

# Стандартизируем (шкалируем) данные к [0;1]
# после нормализации
# maxs <- apply(train[ , с(25)], 2, max) 
# mins <- apply(train[ , с(25)], 2, min)
# train.sc <- scale(train[ , с(25)], center = mins, scale = maxs - mins)

# Разберемся, кто является выбросом

# Есть ли выбросы?
boxplot(log(zzz2)) 


## Преобразование Бокса-Кокса

library(forecast)
# Поиск оптимальной lambda
lambda <- BoxCox.lambda(zzz)
# Преобразование данных
zzz3 <- BoxCox(zzz, lambda)

hist(zzz3)

# Попробуем другой столбец.
zzz <- train[,31]
hist(zzz)

# Поиск оптимальной lambda
lambda <- BoxCox.lambda(zzz)
# Преобразование данных
zzz3 <- BoxCox(zzz, lambda)

hist(zzz3)


## Контроль отрицательных значений 

# Необходим для выполнения сдвига данных в положительную область 
# перед логарифмированием логнормального распределения.
hist(train[,26]) 
boxplot(train[,26]) 
sum((train[!is.na(train[,26]),26]<0))


## Шаг 3. Определение выбросов

# Правило 3-х сигм
# Индикатор выброса:
zzz4 <- as.numeric(abs(zzz2 - mean(zzz2, na.rm = T)) > 3*sd(zzz2, na.rm = T))

# Распределение значений индикатора выброса
table(zzz4)

# Места выбросов заполянются NA
zzz[zzz4==1] <- NA

# Добавим индикатор выбросов zzz4 в данные, как новую колонку


## Автоматизация преобразования количественных переменных (шагов 2-3)
## Не рекомендуется: внутри используется преобразования Бокса-Кокса

length(c(9,24:62))

# Матрица индикаторов выбросов
extremes.ind <- matrix(rep(-99, 50000*40), nrow=50000, ncol=40)
j <- 1

for (i in c(9,24:62)){
  
  # Поиск оптимальной lambda
  lambda <- BoxCox.lambda( train[,i] )
  # Преобразование данных
  zzz3 <- BoxCox(train[,i], lambda)
  
  zzz4 <- as.numeric(abs(zzz3 - mean(zzz3, na.rm = T)) > 3*sd(zzz3, na.rm = T))
  
  zzz4[is.na(zzz4)] <- 0
  
  train[zzz4==1,i] <- NA
  
  extremes.ind[ , j] <- zzz4
  
  j <- j+1
  
}

summary(extremes.ind)


## Шаг 4. Преобразование категориальных переменных: редкие значения. 

# Это - хеши
x0 <- as.character(train$x0)
# Всего их 50000, но уникальных гораздо меньше.
# table() добавляет нам имена категорий
zzz <- table(x0)
# Уникальные хеши
names(zzz)
# Сколько их?
length(zzz)
# Находим хеши, встречающиеся реже чем в 0.5% случаев
zzz.1 <- names(zzz)[which( as.numeric(zzz)/nrow(train)*100 < 0.5 )]
# Помечаем их как редкие
x0[x0 %in% zzz.1 ] <- "Rare"

# В одну строчку
# x0[x0 %in% names(zzz)[as.numeric(zzz)/nrow(train)*100 < 0.5 ] ] <- "Rare"

# Отступление: поэтапное формирование условия "редкости"
plot(as.numeric(zzz))
plot(as.numeric(zzz)/nrow(train)*100)
which( as.numeric(zzz)/nrow(train)*100 < 0.5 )


## Шаг 5. Преобразование категориальных переменных: индикаторы значений. 

# Процедура class.ind из пакета nnet
x0.class <- nnet::class.ind(factor(x0))

# Новым столбцам нужны более красивые имена
x0.class <- as.data.frame(x0.class) # иначе имена не назначатся
names(x0.class)[1:9] <- paste("x0.0", 1:9, sep= "")
names(x0.class)[10:ncol(x0.class)] <- paste("x0.", 10:ncol(x0.class), sep= "")


## Шаг 6. Feature engineering

thr_top <- 0.9
thr_bottom <- 0.05

# names(train) %in% c("x55", "x56", "x57", "x58", "x59", "x60")
# 
# for col in ["x55", "x56", "x57", "x58", "x59", "x60"]:
#   data["mostly_"+col] = (data[col] >= thr_top)*1
#   data["no_"+col] = (data[col] <= thr_bottom)*1


#  paste(n[!n %in% "y1"], collapse = " + ")
#  paste("y1 ~", paste(n[!n %in% "y1"], collapse = " + "))


#  ------------------------------------------------

one<-data.frame(train[,56:61])
sum(one[5,])
check<-data.frame(rep(0,50000),rep(0,50000))

a<-(one[,1])
table(as.numeric(a<0.05))
head(a1)

for (i in 1:6) {
  a<-one[,i]
  a<-as.numeric(a<0.05)
  check[,i]<-a
}

for (i in 1:6) {
  a<-one[,i]
  a<-as.numeric(a>0.90)
  check[,i+6]<-a
}



#  ------------------------------------------------
?match











