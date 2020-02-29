library(imager)

image <- load.image('https://cdn-images-1.medium.com/max/800/0*tk1vtTx6jh0rRv88.png')
plot(image)

data <- locator()
# Esc - для завершения ввода

df <- data.frame(x=data$x, y=data$y)

write.csv(df, 'test_clust.csv', quote = F, row.names = F)
