setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

# Task 1
df1 <- read.table('airpollution.txt', header = TRUE)
df1$NOxPot <- NULL # we do it becaues, NOx & NOxPot are the same columns, so we don't want to multiply yhe same information
## a) Linear model:
m1 <- lm(Mortality~., data = df1)

## b) t-statistic & p-value connected to NOx
## H0: NOx not have an impact on model
## H1: NOx have an impact on model
b_i <- m1$coefficients['NOx']
X <- model.matrix(m1)
n <- nrow(X)
p <- ncol(X)
SSE <- sum((df1$Mortality - m1$fitted.values)^2)
QR <- qr(X)
R <- qr.R(QR)
invR<-solve(R)
XX_inv<-invR%*%t(invR) #(X'X)^-1
t <- b_i/sqrt((SSE/(n-p))*(diag(XX_inv)['NOx'])) #with n-p degrees of freedom
# the same:
summary(m1)$coef[,3]
#p-value for t-statistic
2*pt(-abs(t), n-p)
2*(1-pt(abs(t), n-p))
## by R function:
summary(m1)$coef[,4]['NOx']
### p-value is bigger then 0.05 so we don't have significant signs to reject H0

## c) F-statistic and its p-value
### H0 : B1 = B2 = ... = Bp-1 = 0
### H1 : it's some Bi that is not equal 0
SST <- sum((df1$Mortality - mean(df1$Mortality))^2)
F1 <- ((SST - SSE)/(p-1))/(SSE/(n-p))
## or...
# SSR <- sum((m1$fitted.values - mean(df1$Mortality))^2)
# F <- (SSR/(p-1))/(SSE/(n-p))
# by function:
F2 <- summary(m1)$fstatistic
# p-value:
1 - pf(F1, p-1, n-p)
1 - pf(F2[1], F2[2], F2[3])

### p-value is smaller then 0.05 so we can reject H0

# Task 2 Generate not linear model y = b0 + b1x1^2 + eps
### where b0 = 0.5, b1 = 1, x from uniform distribution with params, eps from N(0,1)
b0 <- 0.5
b1 <- 1
n <- 1000
x <- runif(n, 0, 1)
eps <- rnorm(n, 0, 1)
y <- b0 + b1*(x)^2 + eps

## a) try to fit linear model (it's wrong approach!!!)
m2 <- lm(y~x)
summary(m2)$coef[2,3] #t-statistic
summary(m2)$coef[2,4] #p-value for t-statistic for x
summary(m2)$fstatistic # the same as summary(m2)$coef[2,3]^2
summary(m2)$coef[2,3]^2
1 - pf(summary(m2)$fstatistic[1], summary(m2)$fstatistic[2], summary(m2)$fstatistic[3])
plot(x, y)
abline(m2, col ='red')

# Task 3 (Power of test)
### y = b0 + b1x1 + b2x2 + b3x3 + eps, where b0 = 0.5, b1 = 1, b2 = 0.5, b3 = 0.03
### eps from N(0,1), xi from N(0,1)
n<-100
B<-100
pwr1<-numeric(B)
pwr2<-numeric(B)
pwr3<-numeric(B)
beta<-c(0.5,1,0.5,0.05)
for(b in 1:B){
  X<-matrix(rnorm(n*3),ncol = 3)
  X1<-cbind(1,X)
  eps<-rnorm(n)
  y<-X1%*%beta+eps
  m3<-lm(y~X)
  pvals<-summary(m3)$coef[,4]
  pwr1[b]<-ifelse(pvals[2]<0.05,1,0)
  pwr2[b]<-ifelse(pvals[3]<0.05,1,0)
  pwr3[b]<-ifelse(pvals[4]<0.05,1,0)
}

PWR1<-mean(pwr1)
PWR2<-mean(pwr2)
PWR3<-mean(pwr3)

# b) Plot of power of test t
n <- c(20, 50, 100, 200, 300, 400, 500)
B <- 100
beta<-c(0.5,1,0.5,0.05)
PWR1 <- numeric(length(n))
PWR2 <- numeric(length(n))
PWR3 <- numeric(length(n))
PV1 <- numeric(length(n))
PV2 <- numeric(length(n))
for (j in 1:length(n)){
  pwr1 <- numeric(B)
  pwr2 <- numeric(B)
  pwr3 <- numeric(B)
  pv1 <- numeric(B)
  pv2 <- numeric(B)
  for (b in 1:B){
    X<-matrix(rnorm(n[j]*3),ncol = 3)
    X1<-cbind(1,X)
    eps<-rnorm(n[j])
    y<-X1%*%beta+eps
    m4<-lm(y~X)
    pvals<-summary(m4)$coef[,4]
    pwr1[b]<-ifelse(pvals[2]<0.05,1,0)
    pwr2[b]<-ifelse(pvals[3]<0.05,1,0)
    pwr3[b]<-ifelse(pvals[4]<0.05,1,0)
    pv1[b] <- summary(m4)$coef[2,4]
    pv2[b] <- summary(m4)$coef[4,4]
  }
  PWR1[j] <- mean(pwr1)
  PWR2[j] <- mean(pwr2)
  PWR3[j] <- mean(pwr3)
  PV1[j] <- mean(pv1)
  PV2[j] <- mean(pv2)
  
}
plot(n, PWR1, type = 'b', ylim = c(0,1))
lines(n, PWR2, type = 'b', col = 'red')
lines(n, PWR3, type = 'b', col = 'navy')
PV1
PV2
