mean = 0; sd = 1

op <- par(mfrow=c(2,2))

## alpha = .1 

alpha = .1 

z.half.alpha = qnorm(1-alpha/2)
lb=-z.half.alpha; ub=z.half.alpha

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="x", ylab="", 
     main=paste0("alpha=",alpha), axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)

result <- paste("P(",round(lb,2),"< x <",round(ub,2),") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(-4, 4, 1), pos=0) 

## alpha = .05 

alpha = .05 

z.half.alpha = qnorm(1-alpha/2)
lb=-z.half.alpha; ub=z.half.alpha

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="x", ylab="", 
     main=paste0("alpha=",alpha), axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)

result <- paste("P(",round(lb,2),"< x <",round(ub,2),") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(-4, 4, 1), pos=0) 

## alpha = .01 

alpha = .01 

z.half.alpha = qnorm(1-alpha/2)
lb=-z.half.alpha; ub=z.half.alpha

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="x", ylab="", 
     main=paste0("alpha=",alpha), axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)

result <- paste("P(",round(lb,2),"< x <",round(ub,2),") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(-4, 4, 1), pos=0) 

## alpha = .005  

alpha = .005 

z.half.alpha = qnorm(1-alpha/2)
lb=-z.half.alpha; ub=z.half.alpha

x <- seq(-4,4,length=100)*sd + mean
hx <- dnorm(x,mean,sd)

plot(x, hx, type="n", xlab="x", ylab="", 
     main=paste0("alpha=",alpha), axes=FALSE)

i <- x >= lb & x <= ub
lines(x, hx)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="red")

area <- pnorm(ub, mean, sd) - pnorm(lb, mean, sd)

result <- paste("P(",round(lb,2),"< x <",round(ub,2),") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(-4, 4, 1), pos=0) 

## reset plot options

par(op)

