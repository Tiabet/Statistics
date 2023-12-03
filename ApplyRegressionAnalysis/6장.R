#1
y = c(5,6,8,9,12)
x1 = c(1,200,-30,900,506)
x2 = c(1004,805,1058,200,505)
x3 = c(6.0,7.3,11.3,13.0,13.1)
mydata<-data.frame(y, x1,x2,x3)
library(leaps)
library(car)
x<-cbind(x1,x2,x3)
lp<-leaps(x,y, method="Cp")
lp

#2
attach(table6_1_galapagos)
ratio = Endemics/Species
l.gala = lm(ratio~Area + Elevation + Nearest + Scruz + Adjacent)
summary(l.gala)
l.gala = lm(ratio~Elevation)
cor(table6_1_galapagos[, c("Area", "Elevation", "Nearest", "Scruz", "Adjacent")])
library(MASS)
step <- stepAIC(l.gala, direction="both")

#3
data(longley)
attach(longley)
l.longley = lm(Employed ~ GNP + GNP.deflator + Unemployed + Armed.Forces + Population)
summary(l.longley)
cor(longley[, c("GNP", "GNP.deflator", "Unemployed", "Armed.Forces", "Population")])
library(car)
vif_values <- vif(l.longley)
step <- stepAIC(l.longley, direction="both")
l.longley1 = lm(Employed ~ GNP.deflator + Unemployed + Armed.Forces + Population)
anova(l.longley, l.longley1)
l.longley2 = lm(Employed ~ GNP + GNP.deflator+ Population)
anova(l.longley, l.longley2)
library(corrplot)
cor_matrix <- cor(longley[, c("GNP", "GNP.deflator", "Unemployed", "Armed.Forces", "Population")])
corrplot(cor_matrix, method = "color")
