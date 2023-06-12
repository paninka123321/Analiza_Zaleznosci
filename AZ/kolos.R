setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

# Zadanie 1
df1 <- read.table('mtcars.txt', header = TRUE)
df1_train <- df1[1:26,]
n <- nrow(df1)
df1_test <- df1[27:n,]

#model liniowy
m1 <- lm(mpg~., data = df1_train)

# weryfikacja hipotezy 
summary(m1)
library(car)
p <- ncol(df1)
C <- matrix(0, nrow = 1, ncol = p)
C[,3] = 1
C[,4] = -1
C[,6] = 1
linearHypothesis(m1, c("1*disp - 1*hp + 1*wt = 0.5"))
linearHypothesis(m1, C, c(0.5))

#nie ma podstaw do odrzucenia hipotezy ze ... prawdziwe
#wartość f-statystyki: 0.2481, p-value: 0.6256

#c)
m2 <- step(m1, direction = "backward", k = 2)
#do modelu wzieliśmy: drat, am, gear, carb

#d) model regresji grzbietowej - metoda kroswalidacji a wiec cv.glmnet
X<- df1_train[,-1]
X <- as.matrix(X)
Y <- df1_train$mpg
library(glmnet)
cv1 <- cv.glmnet(X, Y, alpha = 0, nfolds = 3)
lambda_opt <- cv1$lambda.1se
model_ridge <- glmnet(X, Y, alpha = 0, lambda = lambda_opt)

#f) RMSE:
rmse<- function(x, y){return (mean((x - y)^2))
}

#dla m1:
rmse(df1_test$mpg, predict(m1, df1_test))
rmse(df1_test$mpg, predict(m2, df1_test))
rmse(df1_test$mpg, predict(model_ridge, as.matrix(df1_test[,-1])))
#najmniejsze rmse dla modelu ridge

#zad 2
df2 <- read.table('Glass.txt', header = TRUE)

#a)model regresji logistycznej:
m21 <- glm(Type~Na+Si, data = df2, family = 'binomial')

#b) iloraz szans
#szansa na to ze probka 1 nalezy do 1 typu szkla:
pi <- predict(m21,newdata=data.frame(Na=13.71, Si = 73.01),type="response")  #UWAGA type="response"
a <- pi/(1-pi)
#szansza na to, ze 2 probka nalezy do 1 rodzaju szkla:
pj <- predict(m21, newdata = data.frame(Na = 12.8, Si = 72.77), type = 'response')
b <- pj/(1-pj)
a/b #probka 2 ma wieksza sznase na to, ze nalezy do 1 rodzaju szkla

#c)
m_full <- glm(Type~., data = df2, family = 'binomial')
anova(m_full,m21,  test = "Chisq")
#p-value <0.5 więc mozemy odrzucić hipoteze, że model z podpunktu a jest adekwatny 
# w stosunku do modelu pełnego

#d)
predict(m21, newdata = data.frame(Na = 13.73, Si = 73.02), type = 'response')
predict(m_full, newdata = data.frame(RI = 1.515, Na = 13.73, Mg = 3.88, Al =1.1, Si = 73.02,
                                     K= 0.4, Ca = 8.02, Ba = 0, Fe = 0), type = 'response')
