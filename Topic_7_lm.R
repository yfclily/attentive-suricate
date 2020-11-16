# Topic 7: Linear Models

library(Hmisc)
library(corrplot)
library(MASS)
library(car)

rm(list=ls()) # clean all objects in memory

## Simple linear regression
### `rairuoho` case study
rairuoho<-read.table('Data/rairuoho.txt',header=T, sep="\t", dec=".")
cor.test(rairuoho$day6, rairuoho$day7)
corr<-cor(rairuoho[,1:6])
corr # cor.test does not work on Matrix
p.val<-rcorr(as.matrix(rairuoho[,1:6]))$P
corrplot(corr,type='upper',method='color', addCoef.col = "black", p.mat=as.matrix(p.val), sig.level = 0.05,title = "Correlation Matrix", mar = c(2,0,2,0), diag=F)
plot(rairuoho$day6, rairuoho$day7)
abline(lm(rairuoho$day7~rairuoho$day6), col="red", lwd=2)
# remember `ggplot`
# ggplot(rairuoho, aes(x = day6, y = day7)) + 
#  geom_point() +
#  stat_smooth(method = "lm", col = "red")
### Computation
model1 <- lm(Petal.Width ~ Petal.Length, data = iris)
model1
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +
  geom_point() +
  stat_smooth(method = "lm", col = "blue")
summary(model1)
confint(model1)
sigma(model1)*100/mean(iris$Petal.Width)

## Multiple linear regression
### Computation
model2 <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length + Petal.Width , data = iris)
summary(model2)
summary(model2)$coefficient
confint(model2)
sigma(model2)/mean(iris$Petal.Width)
### Model selection 
####BIC/AIC
model3 <- lm(Petal.Width ~., data = iris[,1:4])
model4 <- lm(Petal.Width ~. -Sepal.Width, data = iris[,1:4])
model5 <-  update(model2,  ~. -Sepal.Length)
BIC(model3); BIC(model4);BIC(model5)
AIC(model3); AIC(model4);AIC(model5)
####Stepwise Selection based on AIC
step <- stepAIC(model3, direction='both')
summary(step)
####Stepwise Selection with BIC
n = dim(iris[,1:4])[1]
stepBIC = stepAIC(model3,k=log(n))
summary(stepBIC)
####Diagnostic/colinearity
model3 <- lm(Petal.Width ~., data = iris[,1:4])
plot(model3,1)
plot(model3,2)
plot(model3,3)
plot(model3,5)

corr<-cor(iris[,1:3])
p.val<-rcorr(as.matrix(rairuoho[,1:3]))$P
corrplot(corr,type='upper',method='color', addCoef.col = "black", p.mat=as.matrix(p.val), sig.level = 0.05,diag=F)
vif(model3)
#remove Petal.Length
model6 <- lm(Petal.Width ~. -Petal.Length, data = iris[,1:4])
vif(model6)
# plot(model6)


