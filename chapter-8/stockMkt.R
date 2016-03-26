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
train.indexSMKT = sample(1:nrow(Smarket),1000)
test.dataSMKT = Smarket[-train.indexSMKT,]
glm.fit1 = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,family = binomial,data=Smarket,subset = train.indexSMKT)
logistic = predict(glm.fit,data.frame(test.dataSMKT),type = "response")
plot(logistic,test.dataSMKT$Direction)
pred = rep("Down",length(logistic))
pred[logistic>.5] = "Up"
#pred = data.frame(logistic,rep("Down",length(logistic)))
table(pred,test.dataSMKT$Direction)

library (MASS)
lda.fit=lda(Direction~Lag1+Lag2 ,data=Smarket ,subset=train.indexSMKT)

qda.fit=qda(Direction~Lag1+Lag2 ,data=Smarket ,subset=train.indexSMKT) 
