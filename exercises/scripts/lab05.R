## Boston

# write.table(Boston, "Boston.data", quote = F, sep = "\t", row.names = F, col.names = T)

my_data <- read.table("Boston.data", sep = "\t", header = T)
summary(my_data)
# Исключим medv
medv <- my_data[,14]
my_data <- my_data[,-14]
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data.sc <- scale(my_data)

# Определение числа кластеров
Ks <- 1:15
withinss <- rep(0, length = length(Ks))
for (i in Ks) {
  withinss[i] <- kmeans(my_data.sc, i, nstart = 25)$tot.withinss
}

plot(withinss, type = "b")
# Ничего не дает

set.seed(123)
cl <- kmeans(my_data.sc, 5, nstart = 25)

# Интерпретация

mean(medv[cl$cluster == 1])
# [1] 12.52564
colMeans(my_data[cl$cluster == 1,])
# crim          zn       indus        chas         nox          rm 
# 19.8201508   0.0000000  18.1379487   0.0000000   0.6735128   6.0542051 
# age         dis         rad         tax     ptratio       black 
# 90.4025641   1.9430103  23.5128205 659.2564103  20.0589744  80.7282051 
# lstat 
# 20.9992308 

mean(medv[cl$cluster == 2])
# [1] 20.79928
colMeans(my_data[grp == 2,])

mean(medv[cl$cluster == 3])
# [1] 29
colMeans(my_data[grp == 3,])

mean(medv[cl$cluster == 4])
# [1] 28.975
colMeans(my_data[cl$cluster == 4,])
# crim           zn        indus         chas          nox           rm 
# 0.06557571  55.42857143   3.58571429   0.01190476   0.42240238   6.72734524 
# age          dis          rad          tax      ptratio        black 
# 30.03928571   6.98045833   4.19047619 306.58333333  16.90238095 389.03000000 
# lstat 
# 6.19547619 

mean(medv[cl$cluster == 5])
colMeans(my_data[grp == 5,])

## Расходы на питание во Франции

my_data.0 <- read.table("French Food Data.dat", sep = " ", header = T)

my_data <- my_data.0[,-1]
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
type <- my_data[,1]
my_data <- my_data[,-1]
#
my_data.sc <- scale(my_data)

# Определение числа кластеров
Ks <- 1:11
withinss <- rep(0, length = length(Ks))
for (i in Ks) {
  withinss[i] <- kmeans(my_data.sc, i, nstart = 25)$tot.withinss
}

plot(withinss, type = "b")

set.seed(123)
cl <- kmeans(my_data.sc, 4, nstart = 25)

# Интерпретация

# * рабочие - MA, 
# * служащие - EM, 
# * управляющие - CA. 
# Учитывается количество детей: 2, 3, 4, 5

type[cl$cluster == 1]
# CA2 CA3 CA4
colMeans(my_data[cl$cluster == 1,])
# bread vegetables     fruits       meat    poultry       milk       wine 
# 398.3333   799.6667   624.0000  2219.6667  1074.6667   260.6667   352.0000 

type[cl$cluster == 2]
# [1] EM5 CA5
colMeans(my_data[cl$cluster == 2,])
# bread vegetables     fruits       meat    poultry       milk       wine 
# 549.5     1046.0      717.5     2343.0     1030.0      539.5      301.5 

type[cl$cluster == 3]
# [1] MA4 EM4 MA5
colMeans(my_data[cl$cluster == 3,])
# bread vegetables     fruits       meat    poultry       milk       wine 
# 549.6667   711.6667   424.6667  1774.6667   719.6667   436.3333   436.3333 

type[cl$cluster == 4]
# MA2 EM2 MA3 EM3
colMeans(my_data[cl$cluster == 4,])
# bread vegetables     fruits       meat    poultry       milk       wine 
# 354.25     539.50     369.75    1493.00     548.75     282.25     363.75 


