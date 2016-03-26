library(ISLR)
names(Smarket)
summary(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#logistic Regression
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,data=Smarket)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

#data division
set.seed(2)
train.indexSMKT = sample() 
