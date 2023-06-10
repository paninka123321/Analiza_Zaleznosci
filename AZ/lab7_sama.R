# Task 1
df1 <- trees
m1 <- lm(Volume~Girth, data = df1)
m11 <- lm(Volume~., data = df1)
m111 <- lm(Volume~Girth+Height+I(Height*Height), data = df1)

# Czy można uprościć 3 model do 2 lub 2 do 1?
X1 <- model.matrix(m1)
X11 <- model.matrix(m11)
X111 <- model.matrix(m111)

n <- nrow(trees)
p1 <- ncol(X1)
p11 <- ncol(X11)
p111 <- ncol(X111)

#Zbadamy statystyke dla tego, że model 2 jest adekwatny względem 3:
SSEm11 <- sum(m11$residuals^2)
SSEm111 <- sum(m111$residuals^2)
Fs1 <- ((SSEm11 - SSEm111)/(p111 - p11))/(SSEm111/(n - p111))
1 - pf(Fs1, p111 - p11, n - p111)
anova(m111, m11, test = 'F')
#wartość p-value jest wieksza od 0.05 wieć nie ma podstaw do tego aby odrzucic hipoteze zerowa,
# ze m11 jest adekwatny wzgledem m111

#Statystyka dla tego, że model m1 jest adekwatny względem m111
SSEm1 <- sum(m1$residuals^2)
Fs2 <- ((SSEm1 - SSEm111)/(p111 - p1))/(SSEm111/(n - p111))
1 - pf(Fs2, p111 - p1, n - p111)
anova(m111, m1, test = 'F')
#Wartość p-value <0.05 wieć istnieje podstawa do odrzucenia hipotezy, że m1 jest 
# adekwatny względem m111

#Statystyka dla tego, że model m1 jest adekwatny względem m11
Fs3 <- ((SSEm1 - SSEm11)/(p11 - p1))/(SSEm11/(n-p11))
1 - pf(Fs3, p11 - p1, n - p11)
anova(m11, m1, test = 'F')
#Wartość p-value <0.05 wieć istnieje podstawa do odrzucenia hipotezy, że m1 jest
# adekwatny względem m11

# Task 2
library(car)
?linearHypothesis
davis <- Davis
mod.davis <- lm(weight ~ repwt, data=Davis)

#H_0: \beta_0 = 0 ^ \beta_1 = 1
#H_1: \beta_0 != 0 lub \beta_1 != 1
## the following are equivalent:
linearHypothesis(mod.davis, diag(2), c(0,1))
linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
linearHypothesis(mod.davis, c("(Intercept)", "repwt"), c(0,1))
linearHypothesis(mod.davis, c("(Intercept)", "repwt = 1"))


mod.duncan <- lm(prestige ~ income + education, data=Duncan)

#H_0: \beta_1 = \beta_2
#H_1: \beta_1 != \beta_2
## the following are all equivalent:
linearHypothesis(mod.duncan, "1*income - 1*education = 0")
linearHypothesis(mod.duncan, "income = education")
linearHypothesis(mod.duncan, "income - education")
linearHypothesis(mod.duncan, "1income - 1education = 0")
linearHypothesis(mod.duncan, "0 = 1*income - 1*education")
linearHypothesis(mod.duncan, "income-education=0")
linearHypothesis(mod.duncan, "1*income - 1*education + 1 = 1")
linearHypothesis(mod.duncan, "2income = 2*education")
# p-value > 0.05 nie ma podstaw do orzucenia założenia, że beta1 = beta2
C_duncan <- matrix(0, nrow = 1, ncol = 3)
C_duncan[1, 1] <- 0
C_duncan[1, 2] <- -1
C_duncan[1, 3] <- 1
linearHypothesis(mod.duncan, C_duncan, 0)

C1=matrix(0,nrow=1,ncol=3)
C1[1,1]=0
C1[1,2]=1
C1[1,3]=-1
C1
linearHypothesis(mod.duncan,C1,0)

# b)
setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')
df2 <- read.table('ExerciseCholesterol.txt', header = TRUE)
n<-nrow(df2)

#konstruujemy wektory mówiące nam czy prawda, że dana obserwacja należy do danej grupy
int1 <- ifelse(df2$Group==1,1,0)
int2 <- ifelse(df2$Group==2,1,0)
int3 <- ifelse(df2$Group==3,1,0)

Weight1 <- numeric(n)
Weight1[df2$Group==1]<- df2$Weight[df2$Group==1]
Weight2 <- numeric(n)
Weight2[df2$Group==2] <- df2$Weight[df2$Group == 2]
Weight3 <- numeric(n)
Weight3[df2$Group ==3] <- df2$Weight[df2$Group == 3]

#tworzymy nową ramke danych z 7 zmiennymi:
#3 pierwsze odpowaidają za przynaleznosc do danej grupy
#3 następne za wagi osób
#ostatnia za wartość ich HDL
Data_new <- data.frame(int1,int2,int3,Weight1,Weight2,Weight3,HDL=df2$HDL)
m2 <- lm(HDL~.-1, data = Data_new)
C2 <- matrix(0, nrow = 2, ncol = 6)
C2[1,] <- c(0,0,0,1,-1,0)
C2[2,] <- c(0,0,0,1,0,-1)
d = c(0,0)
linearHypothesis(m2, C2, d)
#nie możemy odrzucić hipotezy o tym ze wspólczynniki te byłuy takie same

library(ggplot2)
qplot(Weight,HDL,data=df2,col=as.factor(Group))


