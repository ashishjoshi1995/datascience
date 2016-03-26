  library(MASS)
  library(ISLR)
  fix(Boston)
  lm.fit = lm(medv~lstat,data = Boston)
  confint(lm.fit)
  summary(lm.fit)
  #for lstat we obtained a high t value and a low pvalue, it may matter, 
  #it can be checked by correlation if in multiple LR p value comes high.
  
  #checking age
  lm.fit = lm(medv~age,data = Boston)
  summary(lm.fit)
  #sufficient t value and p value , therefore applying multiple LR
  
  lm.fit = lm(medv~lstat+age,data=Boston)
  summary(lm.fit)
  # p vlue is sufficiently small to consider the model still checking the correlation of vars
  attach (Boston)
  cor(lstat,age)
  
  #dividing data
  train.indexMLR = sample(1:nrow(Boston),400)
  test.dataMLR = Boston[-train.indexMLR,]
  #trying out with all other vars, best strategy
  lm.fit = lm(medv~.,data=Boston,subset = train.indexMLR)
  outcome = predict(lm.fit,data.frame( test.dataMLR))
  cor(outcome,test.dataMLR$medv)
  plot(outcome,test.dataMLR$medv)  
  summary(lm.fit)
  #age and indus have a very high p value thus can be removed
  lm.fit2 = lm(medv~.-age-indus,data=Boston,subset = train.indexMLR)
  outcome2 = predict(lm.fit2,data.frame(test.dataMLR))
  cor(outcome2,test.dataMLR$medv)
  summary(lm.fit2)
  summary(lm.fit2)$sigma
  summary(lm.fit)$sigma
  rss = mean((outcome-test.dataMLR$medv)^2)
  rss2 = mean((outcome2-test.dataMLR$medv)^2)
  rss
  rss2
  #almost identical
  
  
  #trying nonlinear terms
  
  #Qualitative
  library(ISLR)
  
  lm.fit3 = lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
  summary(lm.fit3)
  #price:Age,USYes,Education,Population must be removed
  
  lm.fit3Mod = lm(Sales~.+Income:Advertising-Age-Education-Population,data = Carseats)
  