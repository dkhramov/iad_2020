babyData = read.csv('week_09/data/birthweight_reduced.csv')
head(babyData, n=3)
dim(babyData)

babyData = na.omit(babyData); dim(babyData)

class(babyData$smoker)
babyData$smoker<-factor(babyData$smoker,
                        labels=c('Non-smoker','Smoker'))

# Проверка на мультиколлинеарность

attach(babyData)
pairs(~Birthweight+Gestation+mheight+mppwt,
      main='Birth weight scatterplots',
      col=c('red','blue')[smoker], pch=c(1,4)[smoker])

round(cor(cbind(Birthweight,Gestation,mppwt,mheight)),2)

# Строим регрессионную модель на основе независимых предикторов

reg1<-lm(Birthweight~Gestation+smoker+mppwt)
summary(reg1)

# Проверка предположений
plot(reg1) # Можно построить все графики сразу,
           # а можно каждый по отдельности.
plot(reg1, which = 1)
plot(reg1, which = 2)
hist(resid(reg1), xlim = range(c(-2.5,2.5)),
     main='Histogram of residuals',
     xlab='Standardised residuals',ylab='Frequency')

## Считаем VIF

library(car)
vif(reg1)

# Но:
vif(lm(Birthweight~Gestation+smoker+mheight+mppwt))

# Или с помощью пакета usdm

library(usdm)
independents <- data.frame(cbind(Gestation,smoker,mppwt))
vif(independents)

## Проверяем на выбросы (рычаг и остатки)

plot(reg1, which = 5)

## Автокорреляция остатков: тест Дурбина-Ватсона

# library(car)
dwt(reg1)

