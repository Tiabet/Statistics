#9장
church
attach(church)
plot(length, height, pch=style, sub="style : Gothic or Romanesque")
style <- as.factor(style)
style <- as.numeric(style)-1
l.church = lm(height ~ length + style)
abline(l.church, col = 'blue')
l.church2 = lm(height ~ length)
abline(l.church2, col = 'red')
grid<-seq(200,600, by=10)
church1 = l.church$coefficients[[1]] + l.chruch$coefficients[[2]]*grid + l.chruch$coefficients[[3]]
lines(grid,church1)
church2 = l.church$coefficients[[1]] + l.chruch$coefficients[[2]]*grid
lines(grid,church2)

library(faraway)
data(sexab)
attach(sexab)
csa = as.numeric(csa)-1
csa
plot(cpa,ptsd,pch = csa)
l.cpa = lm(ptsd ~ cpa + csa)
grid = seq(-4,10,1)
cpa1 = l.cpa$coefficients[[1]] + l.cpa$coefficients[[2]]*grid + l.cpa$coefficients[[3]]
cpa2 = l.cpa$coefficients[[1]] + l.cpa$coefficients[[2]]*grid
lines(grid,cpa1)
lines(grid,cpa2)
summary(l.cpa)
l.cpa2 = lm(ptsd ~ cpa)
abline(l.cpa)
l.cpa2 = lm(ptsd ~ cpa)
abline(l.cpa2)
split_ab = split(sexab,csa)
t.test(split_ab$"0"$ptsd,split_ab$"-1"$ptsd)


chick_m = chick[chick$gender=='m',]
chick_f = chick[chick$gender=='f',]
l.chick_m = lm(growth ~ vitamin, data = chick_m)
l.chick_f = lm(growth ~ vitamin, data = chick_f)
plot(chick$vitamin,chick$growth,pch=chick$gender)
abline(l.chick_f)
abline(l.chick_m)
lrtest(l.chick_f, l.chick_m)

attach(turkey)
l.turkey = lm(y ~ x + z1 + z2)
plot(x,y,,pch =Origin)
grid = seq(20,32,1)
turkey_g = l.turkey$coefficients[[1]] + l.turkey$coefficients[[2]]*grid + l.turkey$coefficients[[3]]
turkey_v = l.turkey$coefficients[[1]] + l.turkey$coefficients[[2]]*grid + l.turkey$coefficients[[4]]
turkey_w = l.turkey$coefficients[[1]] + l.turkey$coefficients[[2]]*grid
lines(grid,turkey_g,lty = 1)
lines(grid,turkey_v, lty = 2)
lines(grid,turkey_w, lty = 3)

#10장
library(MPV)
data(table.b7)
table.b7
l.b7 = lm(y ~ x1 + x2 + x3 + x4 + x5, data = table.b7)
summary(l.b7)
library(car)
vif(l.b7)
library(MASS)

attach(fighter)
l.fighter1 = lm.ridge(SPR ~ .,data = fighter, lambda = 0.001)
l.fighter1$coef
select(l.fighter1)

library(lars)
y= SPR
x = cbind(FFD,RGF,PLF,SLF,CAR)
l.fighter2 = lars(x,y, type = "lasso", trace = TRUE)
plot(l.fighter2, xvar="norm", lwd=2, lty=1:5, breaks=F, col=1:5)
plot(l.fighter2)
coef(l.fighter2)
coefs <- l.fighter2$beta
coefs <- scale(coefs, center=FALSE, 1/l.fighter2$normx)
s1 <- apply(abs(coefs), 1, sum)
s1/max(s1)

attach(drill)
drill
ridge.drill = lm.ridge(Y~S+F+D,lambda = 2.246)
ridge.drill$coef
sum((Y - ridge.drill$coef[1]*S+ridge.drill$coef[2]*F+ridge.drill$coef[3]*D)^2)

y = Y
x = cbind(S,F,D)
lasso.drill = lars(x,y,type="lasso",trace = TRUE)
lasso.drill$RSS
lasso.drill$beta
coef(lasso.drill)
