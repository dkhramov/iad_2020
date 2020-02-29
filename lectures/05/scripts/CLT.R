## Нормальное распределение

mean <- 15.4; sd <- 2.5;

op <- par(mfrow=c(2,2))

pop <- rnorm(1000, mean, sd)
hist(pop, xlab=paste0("sd=", sd))

sample.100 <- rnorm(100, mean, sd)
hist(sample.100)

sample.means.100 <- rep(NA, 1000)
for (i in 1:1000) {
  sample.100 <- rnorm(100, mean, sd)
  sample.means.100[i] <- mean(sample.100)
}
sigma.100 <- round(sqrt(var(sample.means.100)), 3)
hist(sample.means.100, xlab=paste0("sd=", sigma.100))


sample.means.400 <- rep(NA, 1000)
for (i in 1:1000) {
  sample.400 <- rnorm(400, mean, sd)
  sample.means.400[i] <- mean(sample.400)
}
sigma.400 <- round(sqrt(var(sample.means.400)), 3)
hist(sample.means.400, xlab=paste0("sd=", sigma.400))

par(op)

## Равномерное распределение

mean <- 15.4; sd <- 2.5;

d <- sd*sqrt(12)/2;

op <- par(mfrow=c(2,2))

pop <- runif(1000, min=mean-d, max=mean+d)
hist(pop, xlab=paste0("sd=", sd))

sample.100 <- runif(100, min=mean-sd, max=mean+sd)
hist(sample.100)

sample.means.100 <- rep(NA, 1000)
for (i in 1:1000) {
  sample.100 <- runif(100, min=mean-d, max=mean+d)
  sample.means.100[i] <- mean(sample.100)
}
sigma.100 <- round(sqrt(var(sample.means.100)), 3)
hist(sample.means.100, xlab=paste0("sd=", sigma.100))


sample.means.400 <- rep(NA, 1000)
for (i in 1:1000) {
  sample.400 <- runif(400, min=mean-d, max=mean+d)
  sample.means.400[i] <- mean(sample.400)
}
sigma.400 <- round(sqrt(var(sample.means.400)), 3)
hist(sample.means.400, xlab=paste0("sd=", sigma.400))

par(op)

## Экспоненциальное распределение

mean <- 15.4

op <- par(mfrow=c(2,2))

pop <- rexp(1000, 1/mean)
hist(pop)

sample.100 <- rexp(100, 1/mean)
hist(sample.100)

sample.means.100 <- rep(NA, 1000)
for (i in 1:1000) {
  sample.100 <- rexp(100, 1/mean)
  sample.means.100[i] <- mean(sample.100)
}
sigma.100 <- round(sqrt(var(sample.means.100)), 3)
hist(sample.means.100, xlab=paste0("sd=", sigma.100))


sample.means.1000 <- rep(NA, 1000)
for (i in 1:1000) {
  sample.1000 <- rexp(1000, 1/mean)
  sample.means.1000[i] <- mean(sample.1000)
}
sigma.1000 <- round(sqrt(var(sample.means.1000)), 3)
hist(sample.means.1000, xlab=paste0("sd=", sigma.1000))

par(op)

