trainset = apply(row.train[, c(9, 24:62)], 2,NA.to.MEAN)

# Шкалируем к минимуму 0 и максимуму 1
maxs <- apply(trainset, 2, max)
mins <- apply(trainset, 2, min)
trainset <- scale(trainset, center = mins, scale = maxs - mins)

# Кластеризация методом к-средних. Важная опция  nstart=...
k.means = kmeans(trainset, 7, iter.max = 100, nstart=30)
k.means$cluster

y.train = row.train$y
klast = rep(0, 50000)
for (i in 1:7) {
  klast[k.means$cluster == i] = mean(y.train[k.means$cluster == i])
}
klast
unique(klast)
trainset = cbind(trainset, klast)