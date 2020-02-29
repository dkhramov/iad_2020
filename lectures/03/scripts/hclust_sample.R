df <- data.frame(v1=c(1,2,4,5,7), v2=c(1,1,5,7,7))
plot(df[,2], df[,1], xlab="v2", ylab="v1", pch=19)
text(df[,2], df[,1], labels = row.names(df), pos = c(4,4,4,2,2), cex=1.5)
# cex - масштаб отображения по отношению к 1
# pos - расположение подписи к точке:
# 1 = внизу
# 2 = слева
# 3 = вверху
# 4 = справа     

clusters <- hclust(dist(df))
# hang = -1 - ветви дендрограммы на одной высоте
plot(clusters, hang = -1)
