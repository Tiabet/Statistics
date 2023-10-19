#3.4
x<-runif(100,0,10)
y<-2+3*x+rnorm(100,0,1)
plot(x,y)
d<-lm(y~x)
cor(x,y)
abline(d)
summary(d)
confint(d, level = 0.95)
library(lmtest)
dwtest(d)
qqnorm(d$residuals)
esiduals(d)
plot(x,residuals(d))

#3.5
table3_5_campaign 
x<-table3_5_campaign$X
y<-table3_5_campaign$Y
plot(x,y)
cor(x,y)
l = lm(y~x)
abline(l)
summary(l)
confint(l, level=0.95)
residuals(l)
sum(residuals(l)^2)
plot(x,residuals(l))
dwtest(l)
qqnorm(residuals(l))
predict(l,newdata = data.frame(x=50))

#3.6
library(faraway)
data("stat500")
plot(final~midterm, stat500)
abline(0,1)
cor(stat500$final,stat500$midterm)
l1=lm(final~midterm,data = stat500)
l2<-lm(stat500$final~stat500$midterm -1)
summary(l1)
attach(stat500)
plot(final~midterm)
matplot(midterm,predict(l1),type='l')
lines(midterm,data.frame(predict(l1),predict(l2)),type='l',pch = c(1,2),col = c('red','blue'))
lines(midterm,predict(l1),col='red')
lines(midterm,predict(l2),col='blue')
summary(l1)
summary(l2)

#3.7
data(women)
attach(women)
plot(weight,height)
l = lm(height~weight)
abline(l)
summary(l)
plot(l)

#3.9
catfish = table3_6_catfish
attach(catfish)
plot(Y~X)
cor(X,Y)
l = lm(Y~X)
abline(l)
summary(l)
confint(l,level=0.95)
residuals(l)
sum(residuals(l)^2)
dwtest(l)
qqnorm(residuals(l))
l$coefficients[1] + 5.5*l$coefficients[2]

#3.10
attach(textile)
plot(y~x)
l=lm(y~x)
summary(l)
predict(l)
residuals(l)
plot(x,residuals(l))
dwtest(l)
qqnorm(residuals(l))
predict(l,newdata=data.frame(x=400))

#3.12
cars
attach(cars)
summary(lm(dist~speed))
summary(lm(dist~poly(speed,2)))
summary(lm(dist~0+speed))

#3.13
attach(weight)
plot(y~x)
lm(y~x)
lm(y~poly(x,2))
lm(y~poly(x,3))

#3.16
plot(dist~speed)
l=lm(dist~(speed<=15)*speed+(speed>15)*speed)
summary(l)
io<-order(speed)
lines(speed[io],fitted(l,list(speed))[io])
