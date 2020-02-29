## Меры центральной тенденции: среднее и медиана

height <- c(174, 162, 188, 192, 165, 168, 174)
stripchart(height, method = "stack", offset = .5, at = .15, pch = 19)
mean(height)
sum(height)/length(height)
median(height)

order(height)
height[order(height)]

height <- c(height, 225)
mean(height)
median(height)
mean(height, trim=1/8)

## Меры разброса: стандартное отклонение и квантили

var(height)
sum((height-mean(height))^2)/(length(height)-1)

sd(height)
sqrt( sum((height-mean(height))^2)/(length(height)-1) )

summary(height)

max(height) - min(height)
IQR(height)

summary(height)
height[order(height)]

