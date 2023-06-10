setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

#Task 1
df1 <- read.table('uscrime.txt', header = TRUE)
L0 <- lm(R~.-Ex0, data = df1)
L1 <- lm(R~ Age + Ed + Ex1 + U2 + W + X, data = df1)
L2 <- lm(R~ Ed + Ex1, data = df1)
# oszaciwanie błędu predykcji metodą leave-one-out
n <- nrow(df1)
errL0 <- numeric(n)
errL1 <- numeric(n)
errL2 <- numeric(n)

for(i in 1:n){
  L0_cross <- lm(R~.-Ex0, data = df1, subset = -i)
  L1_cross <- lm(R~ Age + Ed + Ex1 + U2 + W + X, data = df1, subset = -i)
  L2_cross <- lm(R~ Ed + Ex1, data = df1, subset = -i)
  errL0[i] <- (df1$R[i] - predict(L0_cross, df1[i,]))^2
  errL1[i] <- (df1$R[i] - predict(L1_cross, df1[i,]))^2
  errL2[i] <- (df1$R[i] - predict(L2_cross, df1[i,]))^2
}
mean(errL0)
mean(errL1)
mean(errL2)

median(errL0)
median(errL1)
median(errL2)

boxplot(errL0-errL1, errL2-errL1)

# Task 2
df2 <- longley
m2 <- lm(Employed~., data = df2)

#estymator ridge
X <- as.matrix(df2[,-7]) #bez intercepta!!!
n <- nrow(X)
p <- ncol(X)
Y <- df2$Employed
#Krok1: przeskalowanie zmiennych
meanX <- colMeans(X)
stdX <- sqrt(apply(X, 2, var)*(n-1)/n)
X_scaled <- matrix(0,ncol = p, nrow = n)
for(i in length(meanX)){
  X_scaled[,i] <- (X[i] - meanX[i])/stdX[i]
}
Y_cen <- Y - mean(Y)

#Ustalenie lambda
lambda <- 10

#Skorzystanie z równania na ridge:
beta_scaled <- solve(t(X_scaled)%*%X_scaled+lambda*diag(p),t(X_scaled)%*%Y_cen)
#same: solve(t(X_scaled)%*%X_scaled+lambda*diag(p))%*%(t(X_scaled)%*%Y_cen)
beta1<-c((mean(Y)-meanX%*%(beta_scaled/stdX))[1,1],beta_scaled/stdX)

#obliczenie ridge za pomoca funkcji wbudowanych
library(glmnet)
sd_y <- sqrt(var(Y)*(n-1)/n)
#alpha =0 dla Ridge, alpha = 1 dla lasso
fit_glment <- glmnet(X, Y, alpha = 0, intercept = TRUE, standardize = TRUE, thresh = 1e-20)
coef(fit_glment)
beta2<-as.vector(coef(fit_glment,s = sd_y*lambda/n,exact = TRUE,x=X,y=Y))
fit_glmnet2<-glmnet(X,Y,alpha = 0,lambda =  sd_y*lambda/n,intercept = TRUE,standardize = TRUE,thresh = 1e-20)
coef(fit_glmnet2)

#c)
model_ridge <- glmnet(X,Y, alpha = 0)
cv1<-cv.glmnet(X,Y,alpha = 0,nfolds = 3)
#optymalne lambda wybrane metoda kroswalidacji 3-krotnej za pomocą pakietu glmnet
lambda_opt <- cv1$lambda.min
cv1$lambda.1se
beta_opt <- as.vector(coef(model_ridge, s = lambda_opt))
#alternatywnie
beta_opt<-coef(model_ridge)[,which(model_ridge$lambda==lambda_opt)]
coef(model_ridge)[2,]
m2$coefficients[2]

plot(rev(coef(model_ridge)[2,]), type = 'l')
#wraz ze wzrostem wartości lambda maleją wartości współczynników

n_lambdas <- length(model_ridge$lambda)
matplot(n_lambdas:1, t(coef(model_ridge))[,-1], type = 'l')
#wraz ze wzrostem lambda wzrastaja maleja wartosci wszystkich wspolczynnikow


beta003<-as.vector(coef(fit_glment,s = sd_y*0.03/n,exact = TRUE,x=X,y=Y))

#Task 3
df3 <- read.table('prostate.txt')
m3 <- lm(lpsa~.-train, data = df3)
m3a <- step(m3, direction = 'backward', k =2)

#dopasowywujemy model używając metody LASSO
X <- as.matrix(df3[,-9])
Y <- df3[,9]
model_lasso <- glmnet(X, Y, alpha = 1)
model_lasso$lambda #są ustawione w kolejności malejącej dlatego trzeba je odwrócić kolejnością
matplot(length(model_lasso$lambda):1, t(coef(model_lasso))[,-1], type = 'l')
#wraz ze wzrostem lambda wartosci wspolczynnikow maleja niektore osiagaja wartosc 0 

cv1<-cv.glmnet(X,Y,alpha=1,nfolds = 3)
cv1$lambda.min
lambda_opt<-cv1$lambda.1se
#d
beta_opt<-as.vector(coef(model_lasso,s=lambda_opt))
#alternatywnie
#beta_opt<-coef(model_lasso)[,which(model_lasso$lambda==lambda_opt)]

# Task 5
#Zad 8.5
library(glmnet)
library(matrixcalc)
p<-10
n<-100
L<-1000
#lambda do glmnet, a nie lambda z wykladu
lambda<-1000
beta<-rep(1,10)

beta_lm<-matrix(numeric(L*p),nrow = p)
beta_ridge<-matrix(numeric(L*p),nrow = p)

for(l in 1:L)
{
  x<-matrix(rnorm(n*p),nrow = n)
  eps<-rnorm(n)
  y<-x%*%beta+eps
  beta_lm[,l]<-lm(y~x-1)$coef
  beta_ridge[,l]<-as.vector(glmnet(x=x,y=y,alpha = 0,intercept = FALSE,lambda = lambda)$beta)
}

bias<-function(x){return(x-beta)}

apply(apply(beta_lm,2,bias),1,mean)
apply(apply(beta_ridge,2,bias),1,mean)
