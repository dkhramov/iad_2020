# Test data

data <- read.csv('data/test_clust.csv')

# K-means

cl <- kmeans(data, 3, nstart = 50)
plot(data, col = cl$cluster)

# DBSCAN

library(dbscan)
kNNdistplot(data, k = 3)
cl.1 = dbscan(data, eps = 15, minPts = 3)
plot(data, col = cl.1$cluster+1L)

# DBSCAN for iris data

data(iris)
iris <- as.matrix(iris[,1:4])

kNNdist(iris, k=4, search="kd")
kNNdistplot(iris, k=4)
# the knee is around a distance of .5

cl <- dbscan(iris, eps = .5, minPts = 4)
pairs(iris, col = cl$cluster+1L)
