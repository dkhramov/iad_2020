## USArrest

# Описание переменных:
# * Murder: аресты за убийство (на 100 000)
# * Assault: аресты за разбойное нападение (на 100 000)
# * UrbanPop: процент городского населения в штате
# * Rape: аресты за насилие (на 100 000)

# Remove missing data
# Scale variables to make them comparable

# Load data
data("USArrests")
my_data <- USArrests
# Remove any missing value (i.e, NA values for not available)
my_data <- na.omit(my_data)
# Scale variables
my_data <- scale(my_data)

# Compute dissimilarity matrix
d <- dist(my_data, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
# Visualize
plot(res.hc, cex = 0.6) # plot tree
rect.hclust(res.hc, k = 4, border = 2:5) # add rectangle

boxplot(USArrests)

bp <- boxplot.stats(USArrests$Rape)
USArrests[USArrests$Rape %in% bp$out,]
# Murder Assault UrbanPop Rape
# Alaska   10.0     263       48 44.5
# Nevada   12.2     252       81 46.0
# - лидеры по насилию.


# Интерпретация кластеров

colMeans(USArrests)
# Murder  Assault UrbanPop     Rape 
# 7.788  170.760   65.540   21.232 

colMeans(USArrests[rownames(my_data) %in% names(grp)[grp == 1],])
# Murder   Assault  UrbanPop      Rape 
# 14.67143 251.28571  54.28571  21.68571 

colMeans(USArrests[rownames(my_data) %in% names(grp)[grp == 2],])
# Murder   Assault  UrbanPop      Rape 
# 10.96667 264.00000  76.50000  33.60833 

colMeans(USArrests[rownames(my_data) %in% names(grp)[grp == 3],])
# Murder    Assault   UrbanPop       Rape 
# 6.210526 142.052632  71.263158  19.184211 

colMeans(USArrests[rownames(my_data) %in% names(grp)[grp == 4],])
# Murder   Assault  UrbanPop      Rape 
# 3.091667 76.000000 52.083333 11.833333 

# Сравним с результатами без стандартизации

d <- dist(USArrests, method = "euclidean")
res.hc <- hclust(d, method = "ward.D2" )
grp <- cutree(res.hc, k = 3)
plot(res.hc, cex = 0.6) # plot tree
# Стандартизация нужна!

## Экономика городов

my_data <- read.csv2("Econom_Cities_data.csv")
summary(my_data)
# Удаляем пропуски данных
my_data <- my_data[my_data$Work != -9999,]

# * City (Город):  Название города
# * Работа (Work): Взвешенное среднее количества рабочих часов, 
#                  сосчитанное по 12 профессиям 
# * Цена (Price):  Индекс цен 112 товаров и услуг (для Цюриха взято за 100%) 
# * Заработная плата (Salary): Индекс заработной платы за час работы 
#                              (для Цюриха взято за 100%)

# Scale variables
my_data.sc <- scale(my_data[,-1])
rownames(my_data.sc) <- my_data[,1]

# Compute dissimilarity matrix
d <- dist(my_data.sc, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2" )
# Visualize
plot(res.hc, cex = 0.6) # plot tree
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
rect.hclust(res.hc, k = 4, border = 2:5) # add rectangle

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 1],-1])
# Work      Price     Salary 
# 1792.00000   77.52632   55.15789 
my_data[rownames(my_data.sc) %in% names(grp)[grp == 1],1]
# [1] Amsterdam   Brussels    Chicago     Copenhagen  Dublin      Dusseldorf 
# [7] Frankfurt   Houston     London      Los_Angeles Luxembourg  Madrid     
# [13] Milan       Montreal    New_York    Paris       Sydney      Toronto    
# [19] Vienna   

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 2],-1])
# Work      Price     Salary 
# 1798.85714   50.94286   19.21429 
my_data[rownames(my_data.sc) %in% names(grp)[grp == 2],1]
# [1] Athens         Lagos          Lisbon         Nicosia        Rio_de_Janeiro
# [6] San_Paulo      Seoul     

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 3],-1])
# Work      Price     Salary 
# 2082.35714   53.12143   14.91429 
my_data[rownames(my_data.sc) %in% names(grp)[grp == 3],1]
# [1] Bogota       Bombay       Buenos_Aires Caracas      Hong_Kong   
# [6] Johannesburg Kuala_Lumpur Manila       Mexico_City  Nairobi     
# [11] Panama       Singpore     Taipei       Tel_Aviv  

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 4],-1])
# Work   Price  Salary 
# 1780.50  108.55   71.30
my_data[rownames(my_data.sc) %in% names(grp)[grp == 4],1]
# [1] Geneva    Helsinki  Oslo      Stockholm Tokyo     Zurich 

## Занятость в странах Европы

# * Country: Страна 
# * Agr: Процент занятых в сельском хозяйстве 
# * Min: Процент занятых в горно-добывающей промышленности
# * Man: Процент занятых в производстве
# * PS: Процент занятых в энергетике
# * Con: Процент занятых в строительстве
# * SI: Процент занятых в сфере услуг
# * Fin: Процент занятых в финансовом секторе 
# * SPS: Процент занятых в социальных службах
# * TC: Процент занятых в транспорте и связи

my_data <- read.csv2("European Jobs_data.csv")
my_data[,-1] <- apply(my_data[,-1], 2, as.numeric)
summary(my_data[,-1])

my_data.sc <- scale(my_data[,-1])
rownames(my_data.sc) <- my_data[,1]

# Compute dissimilarity matrix
d <- dist(my_data.sc, method = "euclidean")
# Hierarchical clustering using Ward's method
res.hc <- hclust(d, method = "ward.D2")
# Visualize
plot(res.hc, cex = 0.6) # plot tree
# Cut tree into 4 groups
grp <- cutree(res.hc, k = 4)
rect.hclust(res.hc, k = 4, border = 2:5) # add rectangle

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 1],-1])
# Agr        Min        Man         PS        Con         SI        Fin 
# 9.5928571  0.8500000 27.6214286  0.9571429  8.4214286 16.6785714  5.1142857 
# SPS         TC 
# 24.0785714  6.7000000 
my_data[rownames(my_data.sc) %in% names(grp)[grp == 1],1]
# [1] Belgium        Denmark        France         W_Germany      Ireland       
# [6] Italy          Luxembourg     Netherlands    United_Kingdom Austria       
# [11] Finland        Norway         Sweden         Switzerland

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 2],-1])
# Agr        Min        Man         PS        Con         SI        Fin 
# 29.3142857  1.3714286 26.3571429  0.6571429  8.8857143  8.8571429  2.4285714 
# SPS         TC 
# 15.5857143  6.5428571 
my_data[rownames(my_data.sc) %in% names(grp)[grp == 2],1]
# [1] Greece   Portugal Spain    Bulgaria Poland   Rumania  USSR 

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 3],-1])
# Agr   Min   Man    PS   Con    SI   Fin   SPS    TC 
# 57.75  1.10 12.35  0.60  3.85  5.80  6.20  8.60  3.60
my_data[rownames(my_data.sc) %in% names(grp)[grp == 3],1]
# [1] Turkey     Yugoslavia

colMeans(my_data[rownames(my_data.sc) %in% names(grp)[grp == 4],-1])
# Agr       Min       Man        PS       Con        SI       Fin 
# 14.133333  2.966667 35.433333  1.466667  8.166667  9.933333  1.000000 
# SPS        TC 
# 19.066667  7.800000 
my_data[rownames(my_data.sc) %in% names(grp)[grp == 4],1]
# [1] Czechoslovakia E_Germany      Hungary

# Карта была бы очень полезна в этой работе.
