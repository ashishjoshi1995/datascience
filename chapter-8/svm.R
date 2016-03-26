set.seed(1)
x= matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]<-x[y==1,]+1
plot(x, col=(3-y))
hist(x)

dat = data.frame(x=x,y=as.factor(y))
dat
library(e1071)
svmfit = svm(y~.,data=dat,kernel="linear",cost=10,scale=T)
plot(svmfit,dat)
summary(svmfit)
names(svmfit)

set.seed(1)
tune.test = tune(svm,y~.,data = dat,kernel="linear",ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.test)
tunemodel.best = tune.test$best.model

#generating a test dataset
xtest = matrix(rnorm(20*2),ncol=2)
ytest = sample(c(-1,1),20,rep = T)
#this line is written to make the 2 classes linearly seperable
xtest[ytest==1,]=xtest[ytest==1,]+1
test.data4 = data.frame(x=xtest,y=as.factor(ytest))

#predictiom
ypred = predict(tunemodel.best,test.data4)
table(ypred,test.data4$y)

x[y==1,]=x[y==1,]+.5
data2 = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data = data2,kernel='linear',cost = 1e5)
plot(svmfit,data2)

#generating nonlinear data
set.seed(1)
x = matrix(rnorm(200*2),ncol=2)
x[1:100,] = x[1:100]+2
x[101:150] = x[101:150]-2
y = c(rep(1,150),rep(2,50))
data.train1 = data.frame(x=x,y=as.factor(y))
plot(x,col=y)
train=sample (200,100) 
svmfit=svm(y~., data=data.train1[train ,], kernel="radial", gamma=1, cost=1) 
plot(svmfit , dat[train ,])
tune.out=tune(svm , y~., data=data.train1[train ,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4))) 
summary(tune.out)

#ROC Curves

