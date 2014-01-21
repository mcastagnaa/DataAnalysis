#2
set.seed(3343)
pValues = rep(NA,100)
for(i in 1:100){
  z = rnorm(20)
  x = rnorm(20)
  y = rnorm(20,mean=0.5*x)
  pValues[i] = summary(lm(y ~ x))$coef[2,4]
}
alpha <- 0.1
alphaFVER = alpha/100
sum(pValues < alphaFVER)

pValues <- pValues[order(pValues)]
control <- alpha*seq(1,100)/100
sum(pValues < control)

#4
set.seed(1234)
e <- rnorm(100)
x <- rnorm(100)
b0 <- 1
b1 <- 2
y <- b0 + b1 * x + e
lmbase <- lm(y ~ x)
summary(lmbase)
lostDataQntl <- 0.7
quantile(x, lostDataQntl)
quantile(y, lostDataQntl)

xHighOut <- x
xHighOut[xHighOut > quantile(x, lostDataQntl)] <- NA

lmXout <- lm(y ~ xHighOut)
summary(lmXout)

yHighOut <- y
yHighOut[yHighOut > quantile(y, lostDataQntl)] <- NA

lmYout <- lm(yHighOut ~ x)
summary(lmYout)

#5
library(MASS)
rlmbase <- rlm(y ~ x)
rlmXout <- rlm(y ~ xHighOut)
rlmYout <- rlm(yHighOut ~ x)

summary(rlmbase)
summary(rlmXout)
summary(rlmYout)