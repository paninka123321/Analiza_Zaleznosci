setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

# Task1
df1 <- longley
## a
m1 <- lm(Employed~., data = df1)

## b) correlation matrix
Xx <- model.matrix(m1, df1)
X <- as.matrix(df1[,-ncol(longley)])
pairs(X) #scatterplot
b <- round(cor(X),3) #correlation matrix

## r squared 
r2 <- sum((m1$fitted.values - mean(df1$Employed))^2)/sum((df1$Employed - mean(df1$Employed))^2)
summary(m1)$r.squared
R2 <- numeric(ncol(X))
for(i in 1:ncol(X)){
  m <- lm(X[,i]~X[,-i], data = as.data.frame(X))
  R2[i] <- summary(m)$r.squared
}
R2
## VIF
VIF <- 1/(1-R2)
VIF #najwiekszy wspolczynnik vif ma gnp co zonacza ze najlepiej jest gnp opisywane przez inne zmienne objasniajace
# występuje duża współliniowość danych


# Task 2
df2 <- read.table('uscrime.txt', header = TRUE)
## a) linear model
m2 <- lm(R~., df2)
X2 <- model.matrix(m2) #z interceptem
X2 <- X2[,-1]
pairs(X2) #wydaje się, że EX0 i EX1 będą współliniowe
c <- cor(X2) # te zmienne mają dużą korelację
## usuwamy jedną ze zmiennych które były najsilniej ze sobą skorelowane
m22 <- lm(R~.-Ex0, data = df2)
summary(m2)
summary(m22)

## b) wybór najlepszego zestawu zmiennych
n<- nrow(model.matrix(m22))
p <- ncol(model.matrix(m22))
sse <- sum(residuals(m22)^2)
## za pomocą AIC
AIC(m22)
-2*logLik(m22)+2*(p+1)
### Uwaga funkcja step inaczje oblica AIC:
n*log(sse/n) + 2*p

##za pomocą BIC
BIC(m22)
AIC(m2, k = log(n))
-2*logLik(m22)+log(n)*(p+1)
### Uwaga funkcja step inaczje oblica BIC:
n*log(sse/n)+log(n)*p

## za pomocą modyfikowanego (skorygowanego) R^2
#adj r^2
summary(m22)$adj.r.squared
1 - (1 - summary(m22)$r.squared)*(n-1)/(n-p)

##za pomocą kryterium Mallowsa
sse + summary(m2)$sigma^2 *2*p

## za pomocą metody pełnego przeszukiwania modeli
col_names<-colnames(df2)[-c(1,5)]

formulas<-lapply(
  apply(expand.grid(rep(list(c(FALSE,TRUE)),p-1)),1,function(x){
    return(col_names[x])}),
  function(x){return(paste0("R~",paste0(x,collapse = "+")))})

wyniki<-data.frame(formula_lm = numeric(length(formulas)),
                   AIC = numeric(length(formulas)),
                   BIC = numeric(length(formulas)),
                   adjr2 = numeric(length(formulas)),
                   Mallows = numeric(length(formulas)))
formulas[1] <- "R~1"
sigma2<-summary(m2)$sigma^2
for(i in 2:length(formulas)){
  f<-as.formula(formulas[[i]])
  m<-lm(f,data = df2)
  X<-model.matrix(m)
  pf<-ncol(X)
  SSE<-sum(residuals(m)^2)
  wyniki$formula_lm[i]<-formulas[[i]]
  wyniki$AIC[i]<-n*log(SSE/n)+2*pf
  wyniki$BIC[i]<-n*log(SSE/n)+log(n)*pf
  wyniki$adjr2[i]<-1-(1-summary(m)$r.squared)*(n-1)/(n-pf)
  wyniki$Mallows[i]<-SSE+2*sigma2*pf
}

wyniki[-1,]$formula_lm[wyniki$AIC==min(wyniki$AIC)]
wyniki[-1,]$formula_lm[wyniki$BIC==min(wyniki$BIC)]
wyniki[-1,]$formula_lm[wyniki$adjr2==max(wyniki$adjr2)]
wyniki[-1,]$formula_lm[wyniki$Mallows==min(wyniki$Mallows)]

## c) wybor zestawu najlepszych zmiennych za pomocą funkcji step
###przy użyciu kryterium AIC krok: backward
step(m22, direction = 'backward', k =2)
###R ~ Age + Ed + Ex1 + M + U1 + U2 + W + X od tego momentu przestał się zmieniać AIC 
###dla poszczególnych zmiennych co oznacza, że model już się nie polepsza
###przy uzyciu AIC krok: "forward"
m0 <- lm(R~1, data = df2)
step(m0, direction = "forward", scope = list(lower = m0, upper = m22), k=2)
###R ~ Ex1 + X + Ed + Age + U2 + W od tego momentu przestaje się zmniejszać AIC
###przy uzyciu AIC krok: "both"
a <-step(m22,direction =  'both',k=2)
###R ~ Age + Ed + Ex1 + M + U1 + U2 + W + X
step(m0,direction =  'both',
     scope = list(lower = m0,upper = m22),k=2)
###R ~ Ex1 + X + Ed + Age + U2 + W
 
# Task 3

##BIC
step(m22,direction =  'backward',k=log(n))
m_null<-lm(R~1,data = df2)
step(m_null,direction =  'forward',
     scope = list(lower = m_null,upper = m22),k=log(n))
step(m22,direction =  'both',k=log(n))
step(m_null,direction =  'both',
     scope = list(lower = m_null,upper = m22),k=log(n))

#d
rank(abs(summary(m22)$coef[-1,3])) #uporzadkowanie zmiennych tak, że p-value maleje

L<-list()

L[[1]]<-lm(R~Ex1,data = df2) #Ex1 zmienna najmniej istotna w modelu m22
L[[2]]<-lm(R~Ex1+X,data = df2)
L[[3]]<-lm(R~Ex1+X+Age,data = df2)
L[[4]]<-lm(R~Ex1+X+Age+Ed,data = df2)
L[[5]]<-lm(R~Ex1+X+Age+Ed+U2,data = df2)
L[[6]]<-lm(R~Ex1+X+Age+Ed+U2+U1,data = df2)
L[[7]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W,data = df2)
L[[8]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M,data = df2)
L[[9]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S,data = df2)
L[[10]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N,data = df2)
L[[11]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N+LF,data = df2)
L[[12]]<-lm(R~Ex1+X+Age+Ed+U2+U1+W+M+S+N+LF+NW,data = df2) 

which_min_AIC<-which(sapply(L,AIC)==min(sapply(L,AIC)))
L[[which_min_AIC]]
which_min_BIC<-which(sapply(L,BIC)==min(sapply(L,BIC)))
L[[which_min_BIC]]

# Task3
L<-50
p<-9
betas<-c(1,1,1,0,0,0,0,0,0)

N<-c(25, 50, 75, 100, 125, 150, 175, 200)
probs_mean_AIC<-numeric(length(N))
probs_mean_BIC<-numeric(length(N))

for(n in N){
  probsAIC<-numeric(L)
  probsBIC<-numeric(L)
  
  for(i in 1:L){
    X<-matrix(rnorm(n*p),ncol = p)
    eps<-rnorm(n)
    y<-X%*%betas+eps
    d<-data.frame(X,y)
    m<-lm(y~.,data = d)
    mAIC<-step(m,direction = "backward",trace = FALSE,k=2)
    mBIC<-step(m,direction = "backward",trace = FALSE,k=log(n))
    probsAIC[i]<-setequal(names(coef(mAIC))[-1], c("X1","X2","X3"))
    probsBIC[i]<-setequal(names(coef(mBIC))[-1], c("X1","X2","X3"))
  } 
  
  probs_mean_AIC[which(N==n)] <- mean(probsAIC)
  probs_mean_BIC[which(N==n)] <- mean(probsBIC)
  
}
plot(probs_mean_AIC~N,type = 'b',col = 'magenta',ylim = c(0,1))
lines(probs_mean_BIC~N,type = 'b',col = 'cyan')
mean(c(FALSE, TRUE, FALSE))
