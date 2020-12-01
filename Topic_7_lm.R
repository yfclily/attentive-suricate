# Topic 7: Linear Models

library(Hmisc)
library(corrplot)
library(MASS)
library(car)
library(interactions)
library(yarrr)
library(tidyr)
library(readr)
library(lme4)
library (lmerTest)
library(nlme)

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
  stat_smooth(method = "lm", col = "blue")+
  xlim(0,7)+
  ylim(-1,3)
summary(model1)
confint(model1)
sigma(model1)*100/mean(iris$Petal.Width)

## Multiple linear regression
### Computation
model2 <- lm(Petal.Width ~ Petal.Length + Sepal.Width + Sepal.Length, data = iris)
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

## ANOVA models
iris.lm<-lm(Petal.Width ~ Species, data=iris)
summary(iris.lm)
anova(iris.lm)

pirateplot(formula = time ~ cleaner + type,
           data = poopdeck,
           ylim = c(0, 150),
           xlab = "Cleaner",
           ylab = "Cleaning Time (minutes)",
           main = "poopdeck data",
           back.col = gray(.97), 
           cap.beans = TRUE, 
           theme = 2)

### Full-factorial between-subjects one-way ANOVA
pirateplot(time ~ cleaner, 
           data = poopdeck, 
           theme = 2, 
           cap.beans = TRUE,
           main = "formula = time ~ cleaner")

cleaner.aov <- aov(formula = time ~ cleaner, data = poopdeck) # Step 1: aov object with time as DV and cleaner as IV
summary(cleaner.aov) # Step 2: summary of the anova object
TukeyHSD(cleaner.aov) # Step 3: post-hoc tests
cleaner.lm <- lm(formula = time ~ cleaner, data = poopdeck) # Step 4: regression object
summary(cleaner.lm)

### Full-factorial between-subjects two-way ANOVA
cleaner.type.aov <- aov(formula = time ~ cleaner + type, data = poopdeck)
summary(cleaner.type.aov)
TukeyHSD(cleaner.type.aov)
cleaner.type.lm <- lm(formula = time ~ cleaner + type, data = poopdeck)
summary(cleaner.type.lm)

### ANOVA with interaction term
cleaner.type.int.aov <- aov(formula = time ~ cleaner * type, data = poopdeck)
summary(cleaner.type.int.aov)
cleaner.type.int.lm <- lm(formula = time ~ cleaner * type, data = poopdeck)
summary(cleaner.type.int.lm)

cat_plot(cleaner.type.int.lm, pred = cleaner, modx = type, interval = TRUE)

### ANOVA types
time.lm <- lm(formula = time ~ type + cleaner, data = poopdeck)
time.I.aov <- aov(time.lm) # type I
time.II.aov <- car::Anova(time.lm, type = 2) # type II
time.III.aov <- car::Anova(time.lm, type = 3) # type III

with(poopdeck, table(cleaner, type)) # balanced dataset

### ANOVA object
names(cleaner.type.int.aov)
poopdeck$int.fit <- cleaner.type.int.aov$fitted.values
poopdeck$me.fit <- cleaner.type.aov$fitted.values
mean(abs(poopdeck$int.fit - poopdeck$time))
mean(abs(poopdeck$me.fit - poopdeck$time))

shapiro.test(cleaner.type.int.aov$residuals) # test our residuals vs a normal distribution
bartlett.test(cleaner.type.int.aov$residuals ~ interaction(cleaner, type), data = poopdeck) # test variance of our residuals in the different groups.



### GLMs
#### Poisson distribution
shag <- read.csv("Data/shagLPI.csv", header = TRUE)
shag$year <- as.numeric(shag$year)  #  year should be a numeric variable
shag.hist <- ggplot(shag, aes(pop)) + geom_histogram() 
shag.hist
shag.m <- glm(pop ~ year, family = poisson, data = shag)
summary(shag.m)


shag.p <- ggplot(shag, aes(x = year, y = pop)) +
  geom_point(colour = "#483D8B") +
  geom_smooth(method = glm, colour = "#483D8B", fill = "#483D8B", alpha = 0.6) +
  scale_x_continuous(breaks = c(1975, 1980, 1985, 1990, 1995, 2000, 2005)) +
  labs(x = " ", y = "European Shag abundance")
shag.p

#### Binomial distribution
Weevil_damage <- read.csv("Data/Weevil_damage.csv")
Weevil_damage$block <- as.factor(Weevil_damage$block) # Making block a factor
weevil.m <- glm(damage_T_F ~ block, family = binomial, data = Weevil_damage)
summary(weevil.m)

### Taking account of the random effects
rairuoho<-read.table('Data/rairuoho.txt',header=T, sep="\t", dec=".")
rairuoho$ID<-rownames(rairuoho)
rai<-rairuoho %>% pivot_longer(day3:day8, names_to = "day", values_to = "length")
rai$day<-parse_number(rai$day)
head(rai)

#### Mixed model with lmer
lmer.rai<-lmer(length ~ day + (1|ID), data=rai, REML=TRUE) # REML default
summary(lmer.rai)
anova(lmer.rai)
rand(lmer.rai)

#### Mixed model with `nlme`
lme.rai = lme(length ~ day, random=~1|ID, data=rai)
summary(lme.rai)

