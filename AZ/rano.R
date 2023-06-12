setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')
#Task 1
df1 <- read.table('carseats.txt', header = TRUE)
n <- nrow(df1)
n*80/100
df1_train <- df1[1:320,]
df1_test <- df1[321:n,]

#b) model liniowy
m1 <- lm(Sales~., data = df1_train)
names(df1_train)
c <- c(0,1,2,-1,0,0,0,0,0,0,0, 0)
d <- 0
library(car)
linearHypothesis(m1, c, d)
F <- linearHypothesis(m1, c, d)
1 - pf(F$F[2],1, nrow(df1_train) - ncol(model.matrix(m1)))
# nie mozemy odrzucic hipotezy 0 



#d)
m4 <- step(m1, direction = 'backward', k = log(nrow(df1_train)))
# CompPrice + Income + Advertising + Price + ShelveLoc + Age

#e) model lasso 
library(glmnet)
X <- model.matrix(m1)[,-1]
Y <- df1_train[,1]
X <- as.matrix(X)
m2 <- cv.glmnet(X, Y, alpha = 1, nfolds = 3)       
m3 <- glmnet(X, Y, alpha = 1, lambda = m2$lambda.min)

mse <- function(x,y){mean((x-y)^2)}
mse(df1_test$Sales, predict(m1, df1_test))
mse(df1_test$Sales, predict(m4, df1_test))

mse(df1_test$Sales, predict(m3, makeX(df1_test)[,-c(1,7,12,14)]))

#zadanie 2
df2 <- read.table('diabetes.txt', header = TRUE)
m2 <- glm(Outcome~., data = df2, family = 'binomial')
#b)
m3 <- glm(Outcome~Glucose+Pregnancies, data = df2, family = binomial)
anova(m2, m3, test = 'Chisq')
#model ten nie jest adekwatny

#c
m2$null.deviance - m2$deviance #wartosc statystyki testowej, ze moodel z samym interceptem jest adekwatny w stosunku do m2

#d) test walda
summary(m2)
#H0: beta_i = 0
#H1: beta_i != 0

#e)
pi <- predict(m3, newdata = data.frame(Pregnancies = 4, Glucose = 120), type = 'response')
pi/(1-pi) #bardziej prawdopodobne ze bedzie zdrowa
