mean <- 15.4; sd <- 2.5

op <- par(mfrow=c(2,2))

sample.1000 <- runif(1000, min = mean-sd, max = mean+sd)
hist(sample.1000, main = "Uniform")

sample.1000 <- rexp(1000, 1/15.4)
hist(sample.1000, main = "Exponential")

sample.1000 <- rnorm(1000, mean=mean, sd=sd)
hist(sample.1000, main = "Normal")

par(op)
