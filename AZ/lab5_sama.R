setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

# Task 1
df1 <- read.table('airpollution.txt', header = TRUE)

## a) linear model with NOx and target Mortality
df1$NOxPot <- NULL
x <- df1$NOx
X <- cbind(1,x)
y <- df1$Mortality
m1 <- lm(y~x, data = df1)
## slope of line:
m1$coefficients[2]
## standard Error for NOx:
summary(m1)
SSE <- sum((y - m1$fitted.values)^2)
s2 <- SSE/(length(x)-2)
stderr <- sqrt(s2*solve(t(X)%*%X))
stderr
## t-statistic for NOx
B <- solve(t(X)%*%X)%*%(t(X)%*%y)
p <- ncol(X)
n <- nrow(X)
SSE <- sum((y - m1$fitted.values)^2)
XX_inv <- solve(t(X)%*%X)
t <-B[2,1]/sqrt((SSE)/(n-p)*XX_inv)
## p-value
pv <- 2*pt(-abs(t), n-p)
##plot
plot(x,y)
abline(m1, col = 'red') #p-value is big, but see that MNK line is close to line y = a, this is the reason for big p-value


## b) linear model for log(NOx)
m2 <- lm(y~log(x), data = df1)
summary(m2)$coef
plot(log(x),y)
abline(m2, col = 'red')

## c) residual plot for log(NOx):
plot(m2, 1)
residuals(m2)
r1 <- rstandard(m2) #studentyzowane
r2 <- rstudent(m2) #studentyzowane modyfikowane
r1[abs(r1)>2] #when |r1| is larger then 2 we could say that this observation is an outlier
wh <- which(abs(r1)>2)
# we build new linear model which ignore observations from wh set
m3 <- lm(y~log(x), data = df1, subset = -wh)
summary(m3)$coef
plot(log(x), y)
abline(m2, col = 'red')
abline(m3, col = 'navy')

# Task 2
df2 <- read.table('phila.txt', header = TRUE)
which(is.na(df2), arr.ind = TRUE)
# there is NAs in column 2 (Crime Rate) and 1 NA in column 4,
# because in next subpoint, we will make prediction of Price regarding CrimeRate,
# I delate this several observations
df2 <- df2[!is.na(df2$CrimeRate),]
# a) HousePrice depending on CrimeRate
plot(df2$CrimeRate, df2$HousePrice)
## linear model
m4 <- lm(HousePrice~CrimeRate, data = df2)
abline(m4, col='red')
rs <- rstudent(m4)
wh <- which(abs(rs)>2)
## detection of influential observations
X<- model.matrix(m4)
which(hatvalues(m4)> 2*ncol(X)/nrow(X))

## another method:
which(cooks.distance(m4)>4/(nrow(X) - ncol(X) - 1))

## in both cases 63 obs is noted as influential value, so we reject it from our ser
m5<- lm(HousePrice~CrimeRate, df2, subset = -63)
summary(m5)
plot(df2$CrimeRate, df2$HousePrice)
abline(m4, col = 'red')
abline(m5, col = 'purple')


# Task 3
df3 <- read.table('cellular.txt', header = TRUE)
plot(df3$Period, df3$Subscribers)
m6 <- lm(Subscribers~Period, df3)
summary(m6)
abline(m6)
plot(df3$Period, log(df3$Subscribers))
m7 <- lm(log(Subscribers)~Period, df3)
summary(m7)
abline(m7, col = 'red')

#box cox
library(MASS)
boxcox(m1)
boxcox(m1, lambda = seq(0,1,0.01))
plot(df3$Period, (df3$Subscribers)^0.4)
m8 <- lm((Subscribers)^0.4~Period, df3)
summary(m8)
abline(m8, col = 'red')
