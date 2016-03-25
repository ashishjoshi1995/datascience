library(tree)
library(ISLR)
attach(Carseats)
high = ifelse(Sales>8,"Yes","No")
Carseats = data.frame(Carseats,high)
tree.carseats<-tree(high~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats)

#randomly sampling 200 out of 400 observations from data
#seperating into test and training data

attach(Carseats)
names(Carseats)
set.seed(2)
train.sample = sample(1:nrow(Carseats),200)
train.data = Carseats[train.sample,]
test.data = Carseats[-train.sample,]
tree.carseats =tree(high~.-Sales,Carseats,subset=train.sample)
tree.pred=predict(tree.carseats ,test.data ,type="class")

table(test.data$high,tree.pred)
x<-cv.tree(tree.carseats,FUN = prune.misclass)
x

prune.carseats= prune.misclass(tree.carseats,best = 9)
plot(prune.carseats)
text(prune.carseats)
tree2.pred = predict(prune.carseats,test.data,type = 'class')
table(tree2.pred,test.data$high)
  
#fitting regression tree over Boston data set in library MASS
library(MASS)
set.seed(1)
train.index = sample(1:nrow(Boston),nrow(Boston)/2)
test.data2 = Boston[-train.index,]
tree.boston = tree(medv~.,Boston,subset = train.index)
summary(tree.boston)

plot(tree.boston)
text(tree.boston)

cv.boston = cv.tree(tree.boston)
summary(cv.boston)
#cv.boston2 = cv.tree(tree.boston,FUN = prune.misclass)
#shows error as prune.misclass is only for classification trees
#summary(cv.boston2)

plot(cv.boston$size,cv.boston$dev,type='b')
#5,6,7 look good
#5

prune.boston = prune.tree(tree.boston,best = 5)
plot(prune.boston)
text(prune.boston,pretty = 0)
yhat = predict(tree.boston,data=test.data2)
plot(yhat,test.data2$medv)
cor(yhat,test.data2$medv)
mean(yhat)
mean(test.data2$medv)


#Bagging and Random Forest
library(randomForest)
set.seed(1)
train.index3 = sample(1:nrow(Boston),nrow(Boston)/2)
test.data3 = Boston[-train.index3,]
boston.bagging = randomForest(medv~.,data=Boston,subset = train.index3,mtry=13,importance = T)
predict.bagging = predict(boston.bagging,test.data3)
plot(predict.bagging,test.data3$medv)
abline(0,1)
#RSS
mean((predict.bagging-test.data3$medv)^2)

boston.bagging2 = randomForest(medv~.,data=Boston,subset = train.index3,mtry=6,ntree=25,importance=T)
predict.bagging2 = predict(boston.bagging2,test.data3)
plot(predict.bagging2,test.data3$medv)
mean((predict.bagging2-test.data3$medv)^2)
importance(boston.bagging2)
varImpPlot(boston.bagging2)

#Boosting
library(gbm)
#lambda = 0.001 by default else shrinkage can be set for lambda.
?gbm
set.seed(1)
boost.boston=gbm(medv~.,data = Boston[train.index,],distribution = "gaussian",n.trees = 5000,interaction.depth = 4)