apple<-read.csv(file='d:/R����ȸ��/���α׷�/mcapple.csv', header=T)
attach(apple)

a<-lm(ybar~day, weights=n)   # ����ȸ��
summary(a)
plot(day,ybar)
abline(a)

detach(apple)