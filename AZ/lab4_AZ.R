setwd('h:\\Windows7\\Desktop\\KulczykPaulina\\dane')
library(dplyr)

# Zad.1
df1 <- read.table('realest.txt',header = T)
print(df1)


#a) Definiujemy macierz eksperymentu i zmienna celu
X <- df1 %>% select(!Price)
first_column <- ones(length(X))
X <- cbind(1,X)
X <- as.matrix(X)

y <- df1 %>% select(Price)
y <- unlist(y)

# sposób R-owy
y <- df1$Price
X <- df1[,-1]

#można też automatycznie
m1 <- lm(Price~., data =df1)
X <- model.matrix(m1)


#b) estymatory parametrów:
#t() <- transpozycja, solve() <- odwrotność, bo solve(A,B): AX = B, 
#a solve(A): AX =I
beta <- solve(t(X)%*%X)%*%t(X)%*%y
beta

#SST
SST <- sum((y-mean(y))^2)

#SSR
SSR <- sum((m1$fitted.values-mean(y))^2)
# m1$fitted.values =X%*%m1$coefficients

summary(m1)$coef[,1]

#SSE
SSE <- sum(m1$residuals^2)
SSE <- sum(residuals(m1)^2)
SSE <- sum((m1$fitted.values-y)^2)
summary(m1)$r.squared


#c jesli zmienne objasniajace bylyby w 100% niezalezne to ponizsze obliczenia powinny wyjsc takie same 
m1$coefficients
m2 <- lm(Price~Bedroom, data = df1)
m2$coef

#d przewiduejemy cene domu dla obserwacji: c(1, 3, 1500, 8, 40, 1000, 2, 1, 0)
c(1, 3, 1500, 8, 40, 1000, 2, 1, 0)%*%m1$coefficients
#lub
predict(m1, newdata = data.frame(1, Bedroom = 3, Space = 1500, Room = 8, Lot = 40, Tax = 1000, Bathroom = 2, Garage = 1, 0))

#e
#estymatory najwiekszej wiarygodnosci sa asymptomatycznie nieobciazone i zgodne 
# estymator wariancji = SSE/n, natomiast my będziemy się zajomwac SSE/n-p (bo robimy poprawke na p, dzięki temu jest nieobciążony)
n <- nrow(X)
p <- ncol(X)
SSE/(n-p)
summary(m1)

#Zad3.2
#a na tablicy 

#b
beta_0 = 2 
beta_1 = 0.5
beta_2 = 1
beta_3 = 0.7
beta <- c(beta_0, beta_1, beta_2, beta_3)
p <- length(beta)
X1 <- rnorm(100, 0,1)
X2 <- rnorm(100, 0,1)
X3 <- rnorm(100, 0,1)
X <- cbind(X1, X2)
X <- cbind(X, X3)
X <- matrix(rnorm(100*(p-1), 0, 1), ncol = p-1)
X <- cbind(1, X)
eps <- rnorm(100, 0, 10)

y <- beta_0 + beta_1 * X1 + beta_2 * X2 + beta_3 *X3 +eps
y <- X%*%beta +eps

lm(y~X)

