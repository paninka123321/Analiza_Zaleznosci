setwd('C:\\Users\\Paulina\\Analiza_Zaleznosci')

#task 1.1
df1 <- read.csv('daneSoc.csv',sep = ";",header = T)

#a) base information

##class 
class(df1)

##type
typeof(df1) #data.frame is a list which elements are columns

##size
dim(df1)
nrow(df1)
ncol(df1)

##class & type of attributes
sapply(df1, class)
sapply(df1, typeof)
lapply(df1, class)
lapply(df1, typeof)

#lapply() vs sapply() - lapply gives as result a list of elements when sapply gives vector 

#bonus
lapply(df1, summary) #summary gives base statistics

##headers
df1[0,]


#b) cross tab (attributes - wyksztalcenie & praca)

table(df1$wyksztalcenie, df1$praca)


#c) base statistics describing 'cisnienie skrurczowe' (systolic pressure) for 
  #'męzczyzna' (men) with 'wykształcenie średnie' (secondary education)

v1 <- df1[df1$plec == 'mezczyzna'&df1$wyksztalcenie == 'srednie',]$cisnienie.skurczowe

mean(v1)
sd(v1)
var(v1)
median(v1)
range(v1)
##or...
summary(v1)


#d) boxplot describing distolic pressure (cisnienie.skurczowe) for men 
  #(plec = 'mezczyzna') depending on work (praca)

v2 <- df1[df1$plec == 'mezczyzna',]
boxplot(cisnienie.skurczowe~praca,v2)
##or...
boxplot(v2$cisnienie.skurczowe~v2$praca, xlab = 'work', ylab = 'distolic pressure')


#e) find patients with secondary education (wykszatlcenie = 'srednie') 
  #& diastolic pressure between 140 and 150 (140 <= cisnienie.skurczowe <= 150)

v3 <- df1[df1$wyksztalcenie == 'srednie' &
            df1$cisnienie.skurczowe >= 140 & 
            df1$cisnienie.skurczowe <= 150,]


#f) find patients with the biggest diastolic pressure (cisnienie.skurczowe)

v4 <- df1[df1$cisnienie.skurczowe == max(df1$cisnienie.skurczowe),]
#or...
v4 <- df1[df1$cisnienie.skurczowe == range(df1$cisnienie.skurczowe)[2],]


#g) find patients with diastolic pressure (cisnienie.skurczowe) bigger then 
  #empirical 0.8 quantile of this attribiute

v5 <- df1[df1$cisnienie.skurczowe > quantile(df1$cisnienie.skurczowe, 0.8),]\


#task 1.2

## quantile charts for normal probe 
n10 <- rnorm(10, mean = 0, sd = 1)
n50 <- rnorm(50) #by default mean = o, sd = 1
n100 <- rnorm(100)
n500 <- rnorm(500)

par(mfrow = c(2,2))
qqnorm(n10)
qqline(n10, col = 'red')
qqnorm(n50)
qqline(n50, col = 'red')
qqnorm(n100)
qqline(n100, col = 'red')
qqnorm(n500)
qqline(n500, col = 'red')

## it says if the quantiles from the probe are the same as the theoretical 
  #quantlies, in our case we check the standard normal distribution, if the points are near 
  #the x=y line the distribution are standar normal if not it's diffrent distribution
## qqline is the line which fits to the points

## quantile charts for gamma(2,2) probe in comparison to standard normal distribution
g10 <- rgamma(10, 2, 2)
g50 <- rgamma(50, 2, 2)
g100 <- rgamma(100, 2, 2)
g500 <- rgamma(500, 2, 2)

# now we check with the charts if it's from standard normal distribution
par(mfrow = c(2,2))
qqnorm(g10)
qqline(g10, col = 'red')
qqnorm(g50)
qqline(g50, col = 'red')
qqnorm(g100)
qqline(g100, col = 'red')
qqnorm(g500)
qqline(g500, col = 'red')

##we can see that values from samples are from positive domain only and points
  #on charts creating an arch instead of line x=y, so this probe is not from 
  #standard normal distribution

## quantile charts for Cauchy(0,1) probe in comparison to standard normal distribution
c10 <- rcauchy(10, 2, 2)
c50 <- rcauchy(50, 2, 2)
c100 <- rcauchy(100, 2, 2)
c500 <- rcauchy(500, 2, 2)

# now we check with the charts if it's from standard normal distribution
par(mfrow = c(2,2))
qqnorm(c10)
qqline(c10, col = 'red')
qqnorm(c50)
qqline(c50, col = 'red')
qqnorm(c100)
qqline(c100, col = 'red')
qqnorm(c500)
qqline(c500, col = 'red')

#we see it's not a normal distribution, cauchy distribution has something similar 
  #with normal distribution, but it has 'heavy tails', and we can see that values
  #from the probe sometimes are realy big


#task 1.3 (now we get random observation and we want to check if there is any
  #correlation between them)
df2 <- read.table('skorelowana_probka.txt', header = TRUE)

##a) point plot
par(mfrow = c(1,1))
plot(df2$x, df2$y)

#on the first glance we can see that there is some correlation between probe
  #(only small amount of outliers)


##b) Pearson correlation coefficient
x <- df2$x
y <- df2$y
cov(x, y)/(sd(x)*sd(y))
mean((x-mean(x))*(y-mean(y)))/sqrt(mean((x-mean(x))**2)*mean((y-mean(y))**2))                                 
sum((x-mean(x))*(y-mean(y)))/sqrt(sum((x-mean(x))**2)*sum((y-mean(y))**2))
cor(x, y) #pearson method is by default


##c)

#H_0: real correlation (rho) of x, y is equal 0
#H_1: real correlation (rho) of x, y is not equal 0
#for big n sqrt(n)*(hat(rho) - rho) converge according to distribution to 
#N(0, (1 - rho^2)^2)

n <- length(x)
2*(1 - pnorm(sqrt(n)*cor(x,y))) #normal distribution is symmetric

#p-value is smaller then 0.05 so we could reject H_0, between x and y in our 
#probe is correletion


#d) confidence interval
#now we use the fact that sqrt(n)*(arcth(hat(rho)) - arcth(rho)) -> N(0, 1) 
#converge according to distribution to create 95% confidence interval
tanh(atanh(cor(x,y))+c(-1,1)*qnorm(0.975)/sqrt(n))


#e) ranking x & y
xr<-rank(x) 
yr<-rank(y)

#plot of ranks
plot(xr, yr)
#we can see that now we have the cloud of points (roughly linear) and we haven't
#got outliers

#f) Spearman's rho & Kendall's tau - correlation coefficients based on ranks

##Spearman's rho
sum((xr-mean(xr))*(yr-mean(yr)))/sqrt(sum((xr-mean(xr))**2)*sum((yr-mean(yr))**2))
cor(xr, yr)
cor(x, y, method = 'spearman')

##Kendall's tau
