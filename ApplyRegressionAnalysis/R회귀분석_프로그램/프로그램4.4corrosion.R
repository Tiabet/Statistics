library(faraway)
data(corrosion)
attach(corrosion)

gF<- lm(loss~ Fe, corrosion)                   # 1?? ???? ȸ?͸???
summary(gF)
plot(Fe, loss)
abline(coef(gF))

gp3<- lm(loss~ Fe+I(Fe^2)+I(Fe^3), corrosion)  # 3?? ????ȸ?͸???
summary(gp2)
grid<- seq(0,2, length=50)
plot(loss~Fe, ylim=c(60,150))
lines(grid, predict(gp3, data.frame(Fe=grid)) )

gp5<- lm(loss~Fe+I(Fe^2)+I(Fe^3)+I(Fe^4)+I(Fe^5), corrosion) # 5?? ????ȸ?͸???
summary(gp5)
grid<- seq(0,2, length=50)
plot(loss~Fe, ylim=c(60,150))
lines(grid, predict(gp5, data.frame(Fe=grid)) )
