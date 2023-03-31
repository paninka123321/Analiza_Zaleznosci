setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

# Task 1 
df1 <- read.table('realest.txt', header = TRUE)

## a) Linear Model
### experimental matrix
X<- as.matrix(cbind(1, df1[,-1]))
lm(Price~., data = df1)
# lm(Price~X, data = df1) it's the wrong way because by deafault R take ones column
m1 <- lm(Price~., data = df1) # in linear model when we add ',' by default R 
# takes ones column and another variables
# and now we can take experimental matrix from built model:
X <- model.matrix(m1)

## b) coefficients:
### by definition:
Y <- df1$Price
beta = solve(t(X)%*%X)%*%t(X)%*%Y
# the same beta but counted in another way: solve(t(X)%*%X,t(X)%*%Y)
# with R functions:
coef(m1)
m1$coefficients
summary(m1)$coef[,1]

## SST, SSR, SSE
SST <- sum((Y - mean(Y))^2)
SSR <- sum((predict(m1, df1[,-1]) - mean(Y))^2)
SSE <- sum((Y - predict(m1, df1[,-1]))^2)
# or..
SSR <- sum((m1$fitted.values - mean(Y))^2)
SSE <- sum((m1$fitted.values - Y)^2)
# or..
SSE <- sum((residuals(m1))^2)

## R^2
SSR/SST
1 - SSE/SST
summary(m1)$r.squared

## impact of number of bedrooms in apartment on price while not changing another variables
m1$coefficients
m2<-lm(Price~Bedroom,data = df1) 
m2$coefficients
### with increasing by 1 number of bedroom and not changing another variables we 
# reduce the price, it's because another variables are constant, and we have
# apartment with more bedrooms but the same space, so our apartment is cramped
### but when we build model which depends only on number of bedrooms we can see
# that while we increase number of bedrooms the price also increase

## d) prediction of price for apartment with:
# bedroom: 3
# space: 1500
# room: 8
# Lot: 40
# bathroom: 2
# garage: 1
# tax: 1000
# condition: 0
predict(m1, data.frame(Bedroom = 3, Space = 1500, Room = 8,
                       Lot = 40, Bathroom = 2, Garage = 1, Tax = 1000,
                       Condition = 0))
##or...
m1$coefficients%*%c(1, 3, 1500, 8, 40, 1000, 2, 1, 0) #but the order of variables is important

## e) unbiased estimator of variance
n <- nrow(X)
p <- ncol(X)
SSE/(n-p)
summary(m1)$sigma^2

# Task 2
# in linear model sum of residuals is equal 0 
sum(m1$residuals)
sum(Y - m1$fitted.values)
# X'e = X'(Y-\hat{Y}) = X'(Y-X(X'X)^{-1}X'Y) = X'Y-X'X(X'X)^{-1}X'Y = X'Y-X'Y=0

# b) linear model for: y = b0 + b1x1 + b2x2 + b3x3 + eps
# for b0 = 2, b1 = 0.5, b2 = 1, b3 = 0.7, xi from N(0,1), eps from N(0,10), n = 100
x1 <- rnorm(100)
x2 <- rnorm(100)
x3 <- rnorm(100)
eps <- rnorm(100, 0, 10)
b0 <- 2
b1 <- 0.5
b2 <- 1
b3 <- 0.7
y <- b0 + b1 * x1 + b2 * x2 + b3 * x3 + eps
m3 <- lm(y~., data = data.frame(x1 = x1, x2 = x2, x3 = x3, y = y))
#sum of residuals
sum(y - m3$fitted.values)
## or..
beat <- c(2,0.5,1,0.7)
n <- 100
p <- length(beta)
X <- matrix(rnorm(n*(p-1)), ncol = p-1)
X1 <- cbind(1, X)
Y <- X1%*%beta +eps
m4 <- lm(Y~X)
sum(residuals(m4))

# c) 1000x experiment 
k <- 1000
vars<-numeric(k)
for (i in 1:k){
  X<-matrix(rnorm(n*(p-1), 0, 1),ncol = p-1)
  X1<-cbind(1,X)
  eps<-rnorm(n,0,10)
  y<-X1%*%beta+eps
  m5<-lm(y~X)
  vars[i]<-residuals(m5)%*%residuals(m5)/(n-p)
}
mean(vars)
## we see that value of estimators are a little bit different for each probe,
# but they always close to 100 (their mean is equal ~100), it's because the noise 
# have variance 100 and it's the main factor which impact for residuals variance 
# this indicate that our estimator is unbiased
