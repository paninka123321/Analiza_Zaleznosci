#Zad 1.1
setwd('h:\\Windows7\\Desktop\\KulczykPaulina\\dane')
getwd()
Dane<-read.csv("daneSoc.csv",sep = ";",header = T) #wczytanie pliku, sep mowi jaki znak oznacza rozpoczecie 
#wartosci nowej kolumny w danym wierszu
#a
class(Dane) #klasa obiektu Dane
head(Dane) #wypisanie kilku pierwszych wierszy
typeof(Dane) #typ obiektu Dane
nrow(Dane) #liczba wierszy
ncol(Dane) #liczba kolumn
dim(Dane) #wymiar danych (liczba wierszy i liczba kolumn)
class(Dane$wyksztalcenie) #klasa kolumny wyksztalcenie w Danych, do konkretnej kolumny mozemy sie dostac po znaku $
typeof(Dane$wyksztalcenie) #typ kolumny wyksztalcenie w Danych
sapply(Dane,class) # ramka danych to lista, gdzie jej elementami sa kolumny, funkcja sapply bierze kazda z kolumn
# (elementow listy) i wykonuje na niej funkcje, ktora jest podana jako drugi argument sapply, 
# nastepnie wyniki uzyskane na poszczegolnych kolumnach (elementach listy) laczy w wektor
lapply(Dane,summary) # to samo co sapply, tylko wyniki funkcji na poszczegolnych kolumnach sa przedstawiane jako
# lista a nie wektor. Funkcja summary zwaraca rozne informacje w zaleznosci od tego jaki 
# jest typ danych w danej kolumnie
#b
table(Dane$wyksztalcenie,Dane$praca)# table na dwoch zmiennych typu character zwraca tabele kontyngencji
#c
cis_mez<-Dane[Dane$plec=='mezczyzna'&Dane$wyksztalcenie=='srednie',]$cisnienie.skurczowe #warunek logiczny w nawiasie
#kwadratowym przed przecinkiem
#tyczy wyboru wierszy, a to 
#co po przecinku kolumn

#cis_mez<-Dane$cisnienie.skurczowe[Dane$plec=='mezczyzna'&Dane$wyksztalcenie=='srednie'] #alternatywna metoda na uzyskanie
#tego co wczesniej, tu najpierw 
#wybieramy kolumne i w nia wkladamy
#warunek logiczny
summary(cis_mez) #na zmiennych numerycznych zwraca Min, Max, Q1, Q3 Med i srednia
var(cis_mez) #wariancja probkowa
sd(cis_mez) #odchylenie standardowe probkowe
range(cis_mez) #Min i Max
#d
boxplot(Dane[Dane$plec=='mezczyzna',]$cisnienie.skurczowe~Dane[Dane$plec=='mezczyzna',]$praca) #wykres skrzynkowy, na poczatku
#to co na osi Y po tyldzie 
#to co na osi X
#e
Dane[Dane$wyksztalcenie=='srednie'&
       Dane$cisnienie.skurczowe>=140&
       Dane$cisnienie.skurczowe<=150,]
#f
#Dane[which.max(Dane$cisnienie.skurczowe),] #zle, poniewaz which.max zwrÃ³ci indeks pierwszej wartosci gdzie max 
#na kolumnie wystapil, a nie wszystkich takich obserwacji
Dane[Dane$cisnienie.skurczowe==max(Dane$cisnienie.skurczowe),] #tak wyciagamy wiersze o wszystkich maksymalnych wartosicach
#cisnienia skurczowego
#g
Dane[Dane$cisnienie.skurczowe>quantile(Dane$cisnienie.skurczowe,0.8),]
#bonus
#ilos vs ilos
plot(Dane$cisnienie.skurczowe,Dane$cisnienie.rozkurczowe) #jesli zmienne na obydwu osiach sa numeryczne mamy wykres punktowy

#zasada w plotach jest taka, Å¼e jesli 
#uzywamy tyldy miedzy zmiennymi 
#to najpierw idzie to co na osi Y,
#a po tyldzie ta co na osi X,
#jesli uzywamy przecinka zamiast 
#tyldy to odwrotnie


plot(Dane$cisnienie.rozkurczowe~Dane$cisnienie.skurczowe)
#jakos vs ilos
plot(as.factor(Dane$plec),Dane$cisnienie.skurczowe) # jesli jedna zmienna jest numeryczna a druga kategoryczna mamy boxplot
#jakos vs jakos
plot(as.factor(Dane$plec),as.factor(Dane$wyksztalcenie)) #jesli zmienne na obydwu osiach sa kategoryczne mamy cos na ksztalt
#tablicy kontyngencji

#Zad 1.2
#a
a1<-rnorm(10) #generujemy probke (przedrostek r) z rozkladu normalnego (norm) standardowego licznosci 10 
qqnorm(a1) #qqplot mowi na ile kwantyle z proby sa takie same jak kwantyle z teoretycznego rozkladu z ktorym probke porownujemy
#jesli chodzi o qqnorm porownujemy sie z kwantylami standardowego rozkladu normalnego, jesli punkty ukladaja
#sie wzdluz linii prostej y=x to probka jest z rozkladu standardowego normalnego, jesli wzdluz innej prostej
#to z innego rozkladu normalnego
qqline(a1) #linia dopasowana do punktow
a2<-rnorm(500) #im wieksza proba tym z wieksza dokladnoscia mozemy stwierdzic czy proba jest z rozkladu normalnego czy nie
qqnorm(a2)
qqline(a2)

#b
b1<-rgamma(10,2,2) #tym razem 10cio elementowa prÃ³bka z rozkłdu gamma z parametrami 2,2
qqnorm(b1)
qqline(b1)
b2<-rgamma(500,2,2) 
qqnorm(b2)
qqline(b2) #widaÄ, Å¼e mamy luk zamiast linii i dodatkowo kwantyle na probce empirycznej nie maja ujemnych wartosci

#c
c1<-rcauchy(10) #rozklad cauchy'ego, jest bardzo podobny do normalnego, ale jest bardziej gruboogonowy niz normalny, wiec
#w jego przypadku wieksze pstwo wylosowania liczby znacznie oddalonej od mody rozkladu
qqnorm(c1)
qqline(c1)
c2<-rcauchy(500) #tu to doskonale widac
qqnorm(c2)
qqline(c2)

#Zad 1.3
#a
skor<-read.table("skorelowana_probka.txt",header = TRUE) #ramka danych majaca kolumne x i y
x<-skor$x
y<-skor$y
plot(x,y) #probka silnie skorelowana, jest male skupisko outlierow

#b
cor(x,y) #korelacja probkowa liczona z wbudowanej funkcji
sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2)) #i z definicji (kowariancja probkowa dzielona przez
# pierwiastek iloczynu probkowych wariancji)
#c
alfa = 0.05
#proponowana statystyka sqrt(n)*q
n <- length(x)
T = sqrt(n)*cor(x,y)
#Liczymy pole [pod ogonkiem za statystyką T]
2*(1- pnorm(T))

#d 
kwartyl <- qnorm(0.975) 
lewy <- tanh(-kwartyl/sqrt(n) + atanh(cor(x,y)))
prawy <- tanh(kwartyl/sqrt(n) + atanh(cor(x,y)))

#e
#kiedy zaleznosc jest monotoniczna ale nieliniowa to uzywamy korelacji rangowych - spermana i pearsona
#na razie zakladamy ze wszystkei punkty maja rozne wartosci zarowno na x jak i na y 
#Spearman
rangi_x <- rank(x)
rangi_y <- rank(y)
cor(rangi_x,rangi_y)
plot(rangi_x,rangi_y)
#wbudowany spearman
cor(x, y, method = 'spearman')

#kendall
zl <- 0 
for (i in 1:n)
{
  for(j in i:n)
  {
    zl <- zl + sign((x[i]-x[j])*(y[i]-y[j]))
  }
}
zl/(n*(n-1)/2)

#f
#jakie jest rozwiazanie przy tych samych wartosciach x/y dla spearmana?
x1 <- c(x, x[n])
y1 <- c(y, y[n])
rank(x1)
rank(y1)

#Zadanie 2.1
air <- read.table('airpollution.txt', header = TRUE)
#a
mor <- air$Mortality
edu <- air$Education
cor1 <- cor(mor,edu)

#b
k<-100000
corrs <- numeric(k)
for(i in 1:k)
{
  morper<-sample(mor)
  corrs[i]<-cor(morper,edu)
  
}
#c
hist(corrs)
abline(v = cor1, col = 'red')

#d
(1+sum(abs(corrs)>abs(cor1)))/(1+length(corrs))
#znacznie mniejsze niz p = 0.05 więc silnie skorelowane