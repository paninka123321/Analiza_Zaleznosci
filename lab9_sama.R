setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

#Zadanie 1
df1 <- read.table('Miasta.txt')

# a) standaryzacja zmiennych
Miasta_std<-scale(df1)
Miasta_std<-as.data.frame(Miasta_std)

# b) wyznaczamy kierunki dla których największa zmienność:
M.pc1 <- princomp(~., cor = FALSE, data = Miasta_std[,1:2])
M.pc1$loadings
M.pc1$scores
biplot(M.pc1,choices = 1:2) #biplot
plot(Miasta_std$Price~Miasta_std$Work,asp = 1)
M.pc1$scores
# 1 składowa głowna dla ZUrichu:
0.707*Miasta_std[nrow(Miasta_std), 1] - 0.707*Miasta_std[nrow(Miasta_std), 2]
# c) analiza składowych głównych dla wszystkich zmiennych
M.pc<-princomp(~.,cor = FALSE,data = Miasta_std)
M.pc$scores[1,1]
sum(M.pc$loadings[,1]*Miasta_std[1,]) #1 skladowa glowana dla zmiennej amsterdam

# d) Jaki jest procent wariancji tłumaczony przez poszczególne składowe?
summary(M.pc) #1-sza składowa główna tlumacvzy aż 72% wariancji
plot(M.pc)
# pierwsze składowe główne wyznaczają ponad 90% wariancji, można powiedzieć, że dwie pierwsze idelanie opisują zjawisko

# e) kierunki główne:
M.pc$loadings
#składowe główne:
M.pc$scores

#f) miasto o największej pierwszej składowej głównej:
Miasta_std[which(M.pc$scores[,1] == max(M.pc$scores[,1])),]
biplot(M.pc,choices = 1:2)
#1 skladowa główna wyjasnia najwiekszą wariancje dla pracy, woec duza jej wartosc oznacza ze duzo sie pracuje

#task2
library(faraway)
df2 <- meatspec
X_train <- df2[1:172,]
X_test <- df2[173:215,]
m1 <-lm(fat~., data = X_train)
n_test <- length(X_test$fat)
RMSE <- sqrt(1/n_test * sum((X_test$fat - predict(m1, X_test))^2))

Meat_train<-meatspec[1:172,]
Meat_test<-meatspec[173:nrow(meatspec),]
m1<-lm(fat~.,data = Meat_train)
rmse<-function(x,y){return(sqrt(mean((x-y)^2)))}
rmse(predict(m1,Meat_test),Meat_test$fat)

n <- nrow(X_train)
m2 <- step(m1, direction = 'backward', k = log(n))
rmse(predict(m2, Meat_test), Meat_test$fat)

# c) standaryzacja zxmiennych objaśniających
Meat_train_scaled <- scale(Meat_train[,-101])
Meat_test_scaled <- scale(Meat_test[,-101])
means<-attr(Meat_train_scaled,"scaled:center")
stds<-attr(Meat_train_scaled,"scaled:scale")
#poprawione, bo średnie z traina:
scaled<-scale(Meat_train[,-101])
means<-attr(scaled,"scaled:center")
stds<-attr(scaled,"scaled:scale")
Meat_train_std<-as.data.frame(scaled)
Meat_test_std<-scale(Meat_test[,-101],center = means, scale = stds)

# d) skladowe główne
Meat_train_std <- as.data.frame(Meat_train_scaled)
M.pc2 <- princomp(~., data=Meat_train_std)
plot(M.pc2)
M.pc2$loadings[,1] #absorbancja kazdej fali jest bardzo wazna do liczenia zawartosci tluszczu

#bonus
D<-data.frame(cbind(Meat_train$fat,M.pc2$scores))
colnames(D)[1]<-"fat"
m_bonus<-lm(fat~.,data = D)
beta_pcr<-m_bonus$coef[-1]%*%t(M.pc$loadings)/stds#coefy PCR bez intercepta
beta0_pcr<-m_bonus$coef[1]-sum(means*m_bonus$coef[-1]%*%t(M.pc$loadings)/stds)#intercept
#jseli biorê wszystkie wspó³czynniki z PCR-a to wracam do liniowego

#e)
summary(M.pc2) #pierwsze 13 zmiennych wyjasnia bardzo duzo wariancji
plot(M.pc2)
plot(1:10, M.pc2$sdev[1:10], type = 'l') #patrzac na wykres decydujemy sie na 5 pierwszych skladowych glownych
D<-data.frame(cbind(Meat_train$fat,M.pc$scores[,1:5]))
colnames(D)[1]<-"fat"
#obliczenie skladowych glownych dla zbioru testowego
comp_test<-Meat_test_scaled%*%M.pc$loadings[,1:5]
D_test<-data.frame(cbind(Meat_test$fat,comp_test))
colnames(D_test)[1]<-"fat"
##model liniowy
m_pcr <- lm(fat~., data = D)
rmse(predict(m_pcr, D_test), Meat_test$fat)
#coefy m3 to jeszcze nie beta PCR
#beta PCR
beta_pcr<-m_pcr$coef[-1]%*%t(M.pc2$loadings[,1:5])/stds#coefy PCR bez intercepta
beta0_pcr<-m_pcr$coef[1]-sum(means*m_pcr$coef[-1]%*%t(M.pc2$loadings[,1:5])/stds)#intercept
rmse(as.matrix(Meat_test[,-101])%*%t(beta_pcr)+beta0_pcr,Meat_test$fat)

#f)
#podzial na zbior treningowy, walidacyjny i testowy
Meat_train<-meatspec[1:130,]
Meat_valid<-meatspec[131:172,]
Meat_test<-meatspec[173:nrow(meatspec),]

scaled<-scale(Meat_train[,-101])
means<-attr(scaled,"scaled:center")
stds<-attr(scaled,"scaled:scale")
Meat_train_std<-as.data.frame(scaled)
Meat_valid_std<-scale(Meat_valid[,-101],center = means, scale = stds)
Meat_test_std<-scale(Meat_test[,-101],center = means, scale = stds)

#stworzenie składowych głównych
M.pc<-princomp(~.,data = Meat_train_std)

RMSE<-numeric(100)

for(i in 1:length(RMSE)){
  names1<-c("fat")
  for(k in 1:i){names1<-c(names1,toString(k))}
  D<-as.data.frame(cbind(Meat_train$fat,M.pc$scores[,1:i]))
  names(D)<-names1
  
  #oblicznie i skladowych glownych dla zbioru walidacyjnego
  comp_valid<-as.matrix(Meat_valid_std)%*%M.pc$loadings[,1:i]
  
  #zbiór walidacyjny ze skladowymi głównymi
  D_valid<-as.data.frame(cbind(Meat_valid$fat,comp_valid))
  names(D_valid)<-names1
  
  m3<-lm(fat~.,data = D)
  RMSE[i]<-rmse(predict(m3,D_valid),D_valid$fat)
}
which(RMSE==min(RMSE))
#dla 82 sklaodwych głownych mielismy najmniejsze rmse
names1<-c("fat")
for(k in 1:82){names1<-c(names1,toString(k))}
D<-as.data.frame(cbind(Meat_train$fat,M.pc$scores[,1:82]))
names(D)<-names1

comp_test<-as.matrix(Meat_test_std)%*%M.pc$loadings[,1:82]

D_test<-as.data.frame(cbind(Meat_test$fat,comp_test))
names(D_test)<-names1

m82 <- lm(fat~., data = D)
rmse(predict(m82, D_test), D_test$fat)

which(RMSE[-82] == min(RMSE[-82]))
#Dla 17
names1<-c("fat")
for(k in 1:17){names1<-c(names1,toString(k))}
D<-as.data.frame(cbind(Meat_train$fat,M.pc$scores[,1:17]))
names(D)<-names1

comp_test<-as.matrix(Meat_test_std)%*%M.pc$loadings[,1:17]

D_test<-as.data.frame(cbind(Meat_test$fat,comp_test))
names(D_test)<-names1

m17<-lm(fat~.,data = D)
rmse(predict(m17,D_test),D_test$fat)

#bonus
library(pls)
Meat_train<-meatspec[1:172,]
Meat_test<-meatspec[173:nrow(meatspec),]

#zanjduje optymana liczbe compow przy uzyciu sprawdzianu krzyzowego
meat_pcr<-pcr(fat~.,scale = TRUE,ncomp = 100,data = Meat_train,validation = "CV")

meat_pcr$loadings[,1]
meat_pcr$scores

summary(meat_pcr)
pred<-predict(meat_pcr,newdata = Meat_test,ncomp=20)
rmse(pred,Meat_test$fat) #bardzo zgrabny wynik

#z tego pakietu mozna rowniez dopasowac plsr
