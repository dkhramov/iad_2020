plot.new()
plot.window(xlim = c(0,8), ylim = c(0,8))

## Загрузка фонового изображения

library("png")
pp <- readPNG("data_analysis/week_12/tc05.png")
rasterImage(pp,-1,-1,10,10)

## Интерактивный ввод точек поверх фонового изобразения

coords <- locator(type="p") # add points
coords # display list 

crosses <- coords

coords <- locator(type="p") # add points

zeroes <- coords

## Сохранение точек

crosses.1 <- append(crosses, list(type=rep(1,length(crosses$x))))

crs <- as.data.frame(crosses.1)

zeroes.1 <- append(zeroes, list(type=rep(0,length(zeroes$x))))
zrs <- as.data.frame(zeroes.1)

df <- rbind(crs,zrs)

write.csv(df, file = "twoclasses.csv", quote = F, row.names = F)

## Рисования графика

plot(df$x, df$y, type="n", axes=FALSE, frame.plot=TRUE, xlab="x1", ylab="x2")

# axis(side=2, las=2)
# axis(side=1)

cr <- df$type == 1
zr <- df$type == 0

points(df$x[cr], df$y[cr], pch=3)
points(df$x[zr], df$y[zr], pch=1)

lines(c(5.1,5.1), c(0,9))
mtext("a", side = 1, line = 1, at = c(5.1))

lines(c(0,5.1), c(4.3,4.3))
mtext("b", side = 2, line = 1, at = c(4.3), las = 2)
lines(c(0,5.1), c(6.7,6.7))
mtext("c", side = 2, line = 1, at = c(6.7), las = 2)

lines(c(3.7,3.7), c(6.7,9))
mtext("d", side = 3, line = 1, at = c(3.7))

points(2.5, 5, pch=2)

