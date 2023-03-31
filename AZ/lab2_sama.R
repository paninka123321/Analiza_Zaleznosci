setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

# Task 1
df1 <- read.table('airpollution.txt', header = TRUE)

##a) correlation coefficient between Mortality & Education
cor(df1$Mortality, df1$Education, method = 'pearson')

###by definition
person_cor <- cov(df1$Mortality, df1$Education)/
  (sd(df1$Mortality)*sd(df1$Education))
x <- df1$Mortality
y <- df1$Education
person_cor2a <- (sum((x-mean(x))*(y-mean(y)))/(length(x)-1))/
  (sqrt(sum((x-mean(x))^2)/(length(x)-1)*sum((y-mean(y))^2)/(length(x)-1)))
###after simplification
person_cor2b <- sum((x-mean(x))*(y-mean(y)))/
  sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))

## b) Permutation Test - permutations of Mortality and their correlations
k <- 100000
cors <- numeric(k)
for (i in 1:k){
  x_permutation <- sample(x) #default replace is False
  cors[i] <- cor(x_permutation, y)
}

## c) histogram of cors
hist(cors)
abline(v = cor(x,y), col = 'red')

## d) significance of correletion x, y
sum(abs(cor(x,y)) > (1-0.05)*cors)/k*100
(1+sum(cor(x,y)>abs(cors)))/(1+k)
# so correlation between x, y is significant
# (we add 1 to not have 0 p-value)

## e) now we do the same for JulyTemp & SO2Pot
j <- df1$JulyTemp
s <- df1$S02Pot
cor2 <- cor(j, s)

### Permutation Tes
k <- 100000
cors2 <- numeric(k)
for (i in 1:k){
  j_per <- sample(j)
  cors2[i] <- cor(j_per, s)
}

### Significance
hist(cors2)
abline(v = cor2, col = 'red')
sum(abs(cor2) > (1-0.05)*cors2)/k #it's not equal 100% and is not even close to 100%
(sum(cors2 > abs(cor2)))/(k)
#p-value is bigger then 5%, so we don't reject the hypothesis of non correlation between j and s


# Task 2
## We have Z from N(0,1), X = 2*Z + Nx, Nx from N(0,1), Y = 5*Z + Ny, Ny from N(0,1)
Z <- rnorm(10000)
Nx <- rnorm(10000) 
Ny <- rnorm(10000)
X <- 2*Z + Nx
Y <- -5*Z +Ny

# a) Correlation coefficient between X & Y
cor(X, Y)
## theoretical correlation:
th_cor <- (-10)/sqrt(5*16)

## Partial correlation
library(ppcor)
d <- data.frame(x = X, y = Y,  z = Z)
pcor(d)
##or...
cov(X-2*Z, Y+5*Z)/sqrt(var(X-2*Z)*var(Y+5*Z))
cov(Nx,Ny)/sqrt(var(Nx)*var(Ny))
##or..
m1 <- lm(X~Z-1) #without intercept (beta_0)
m1$coefficients
m2 <- lm(Y~Z-1)
m2$coefficients
cor(X - m1$coef*Z, Y- m2$coef*Z)

# c) Now, we have V = Z^2 + Nv, Nv from N(0,1)
Nv <- rnorm(10000)
V = Z^2 + Nv
cov(V, Y)/sqrt(var(V)*var(Y))
cov(Z^2+Nv-0*Z, -5*Z+Ny+5*Z)/sqrt(var(Z^2+Nv-0*Z)*var(-5*Z+Ny+5*Z))
cor(V, Y)
d2 <- data.frame(Y,V,Z)
pcor(d2)
#now, we can see true relation between V & Y, Z has a huge impact on correlation between V and Y


# Task 3
x <- seq(0,10,0.1)
n <- length(x)
eps1 <- rnorm(n, 0, 3) #vector of noise
y <- x + eps1

plot(x, y)

## a) Correlation coefficient
cor(x, y)
### by def:
sum((x - mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))^2)*sum((y-mean(y))^2))

## b) coefficients of MNK line
b1 <- sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2) 
b0 <- mean(y) - mean(x)*b1
m <- lm(y~x) 
## c)
plot(x, y)
abline(b0, b1, col='red')
abline(m, col='green')

## d)
eps2 <- rnorm(n, 0, 0.5)
eps3 <- rnorm(n,0,5) #vectors of noise
y2 <- x + eps2
y3 <- x + eps3
cor(x,y2)
cor(x,y3)

b1_2 <- sum((x-mean(x))*(y2-mean(y2)))/sum((x-mean(x))^2)
b0_2 <- mean(y2) - mean(x)*b1_2
m2 <- lm(y2~x)
m3 <- lm(y3~x)

par(mfrow=c(2,2))
plot(x,y)
abline(m, col = 'red')
plot(x,y2)
abline(m2, col ='green')
plot(x,y3)
abline(m3, col = 'navy')

#if the noise has small variance the covariance is bigger and coefficients are better fitted
#if the noise has huge variance the covariance is smaller and coefficients are worse fitted
#moreover if the variance is huge the points are further from line 
#if variance is small their are focused near the line

# Task 4
library(MASS)
hills
## a) plots and correlation between time and other varibles
par(mfrow=c(2,1))
plot(hills$dist, hills$time)
plot(hills$climb, hills$time)
cor(hills$dist, hills$time)
cor(hills$climb, hills$time)

## b) MNK line
x <- hills$climb
y <- hills$dist
z <- hills$time

### first by definition
b1_3 <- sum((x-mean(x))*(z-mean(z)))/sum((x-mean(x))^2)
b0_3 <- mean(z) - mean(x)*b1_3

plot(x, z)
abline(b0_3, b1_3, col = 'red')

### abline to second plot by lm
m4 <- lm(z~y)
plot(y, z)
abline(m4, col='green')

### r^2
m3 <- lm(z~x)
SST <- sum((z - mean(z))^2)
SSRm1 <- sum((mean(z) - (b0_3 + b1_3*x))^2)
SSEm1 <- sum((m3$fitted.values - z)^2) # m3$fitted.values it's the same as b0_3 + b1_3*x

SSRm2 <- sum((mean(z) - m4$fitted.values)^2)
ssEm2 <- sum((m4$fitted.values - z)^2)

SSRm1/SST
### or...
1-SSEm1/SST

SSRm2/SST
1 - ssEm2/SST

### r^2 by summary()
summary(m3)$r.squared
summary(m4)$r.squared

### BONUS:
(cor(hills$dist,hills$time))^2 #R^2 in models with one varible is a correlation 
#between varibel and target
summary(m4)$r.squared

(cor(hills$climb,hills$time))^2
summary(m3)$r.squared
#####

# c) time prediction for distance = 15 miles
sum(m4$coefficients*c(1,15)) 
### or...
m5 <- lm(time~dist, data = hills)
predict(m5,data.frame(dist=15))
### Warning! If we crating linear model better option is to use function lm 
# with dataframe param becaues predict(m4, data.frame(dist=15)) will not work

## BONUS:
predict(m5,data.frame(dist=15), interval = 'confidence')


# Task 5 - example that shows before we start making model we should deeply study the data 
# (data exploration phase really important)
df5 <- read.table('anscombe_quartet.txt', header = TRUE)
model1 <- lm(Y1~X1, data = df5)
model2 <- lm(Y2~X2, data = df5)
model3 <- lm(Y3~X3, data = df5)
model4 <- lm(Y4~X4, data = df5)

par(mfrow = c(2,2))
plot(df5$X1, df5$Y1)
abline(model1, col = 'red')
plot(df5$X2, df5$Y2)
abline(model2, col = 'red')
plot(df5$X3, df5$Y3)
abline(model3, col = 'red')
plot(df5$X4, df5$Y4)
abline(model4, col = 'red')

c1 <- cor(df5$X1, df5$Y1)
coef1 <- model1$coefficients
r1 <- summary(model1)$r.squared
c2 <- cor(df5$X2, df5$Y2)
coef2 <- model2$coefficients
r2 <- summary(model2)$r.squared
c3 <- cor(df5$X3, df5$Y3)
coef3 <- model3$coefficients
r3 <- summary(model3)$r.squared
c4 <- cor(df5$X4, df5$Y4)
coef4 <- model4$coefficients
r4 <- summary(model4)$r.squared
data.frame(model = c('model1', 'model2', 'model3', 'model4'),
           cor = c(c1, c2, c3, c4),
           coef_0 = c(coef1[1], coef2[1], coef3[1], coef4[1]),
           coef_1 = c(coef1[2], coef2[2], coef3[2], coef4[2]),
           r_2 = c(r1, r2, r3, r4))
