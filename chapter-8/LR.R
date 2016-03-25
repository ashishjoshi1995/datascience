  library(MASS)
  library(ISLR)
  fix(Boston)
  lm.fit = lm(medv~lstat,data = Boston)
  confint(lm.fit)
  