# Zad.2.2
#a
#korelacja częściowa cor(X,Y|Z)
#korelacja cor(X,Y) = cov(X,Y)/sqrt(Var(X),Var(Y))
-10/sqrt(5*26)
#b
z <-rnorm(10000)
nx <- rnorm(10000)
ny <- rnorm(10000)
x<- 2*z +nx
y<- -5*z + ny
cor(x,y)
library(ppcor)
d <- data.frame(x=x, y=y, z=z)
pcor(d)
m1 <- lm(x~z-1)
m1$coefficients

m2 <- lm(y~z-1)
m2$coefficients

cor(x-m1$coefficients*z,y-m2$coefficients*z)
 
#2.3
x <- seq(0,10,0.1)
eps <- rnorm(101,0,3)
y <- x + eps
plot(x,y)
#a
cor(x,y)
#b
m1<- lm(y~x) #y w zaleznosci od x
abline(m1$coefficients[1], m1$coefficients[2], col = 'red')
#lub
abline(m1, col = 'red')


#2.4 
library(MASS)
par(mfrow=c(1,2))
plot(hills$dist, hills$time)
cor(hills$dist, hills$time)
m1 <- lm(time~dist, data = hills)
abline(m1, col = 'red')
plot(hills$climb, hills$time)
cor(hills$climb, hills$time)
m2 <- lm(time~climb, data = hills)
abline(m2, col = 'red')

SST <- sum((hills$time-mean(hills$time))^2)
SSRm1 <- sum((m1$fitted.values-mean(hills$time))^20)
SSRm2 <- sum((m2$fitted.values-mean(hills$time))^20)
SSEm1 <- sum((m1$fitted.values-hills$time)^20)
SSRm2 <- sum((m2$fitted.values-hills$time)^20)
