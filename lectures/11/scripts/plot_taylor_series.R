x <- seq(-1,2,.1)
y <- exp(x)

xred <- 1

y0 <- rep(xred,length(x))
err0 <- exp(xred) - 1

plot(x,y, type="l", 
     main = paste("y0 = 1,", 
                  "error(1.0) = ", round(err0,3)),
     cex.main = 0.9)
lines(x, y0, col="red")
points(c(0,1), exp(c(0,1)))
points(xred, 1, col="red")

y1 <- 1 + x
err1 <- exp(1) - (1 + 1)

plot(x,y, type="l", 
     main = paste("y1 = 1+x,", 
                  "error(1.0) = ", round(err1,3)),
     cex.main = 0.9)
lines(x, y1, col="red")
points(c(0,1), exp(c(0,1)))
points(xred, 1 + xred, col="red")

y2 <- 1 + x + x^2/2
err2 <- exp(1) - (1 + 1 + 1/2)

plot(x,y, type="l", 
     main = paste("y2 = 1+x+x^2/2,", 
                  "error(1.0) = ", round(err2,3)),
     cex.main = 0.9)
lines(x, y2, col="red")
points(c(0,1), exp(c(0,1)))
points(xred, 1 + xred + xred^2/2, col="red")

y3 <- 1 + x + x^2/2 + x^3/6
err3 <- exp(1) - (1 + 1 + 1/2 + 1/6)

plot(x,y, type="l", 
     main = paste("y3 = 1+x+x^2/2+x^3/6,", 
                  "error(1.0) = ", round(err3,3)),
     cex.main = 0.9)
lines(x, y3, col="red")
points(c(0,1), exp(c(0,1)))
points(xred, 1 + xred + xred^2/2 + xred^3/6, col="red")

y4 <- 1 + x + x^2/2 + x^3/6 + x^4/24
err4 <- exp(1) - (1 + 1 + 1/2 + 1/6 + 1/24)

plot(x,y, type="l", 
     main = paste("y4 = 1+x+x^2/2+x^3/6+x^4/24,", 
                  "error(1.0) = ", round(err4,3)),
     cex.main = 0.9)
lines(x, y4, col="red")
points(c(0,1), exp(c(0,1)))
points(xred, 1 + xred + xred^2/2 + xred^3/6 + xred^4/24, col="red")

y5 <- 1 + x + x^2/2 + x^3/6 + x^4/24 + x^5/120
err5 <- exp(1) - (1 + 1 + 1/2 + 1/6 + 1/24 + 1/120)

plot(x,y, type="l", 
     main = paste("y5 = 1+x+x^2/2+x^3/6+x^4/24+x^5/120,", 
                  "error(1.0) = ", round(err5,3)),
     cex.main = 0.9)
lines(x, y5, col="red")
points(c(0,1), exp(c(0,1)))
points(xred, 1 + xred + xred^2/2 + xred^3/6 + xred^4/24 + xred^5/120, col="red")

