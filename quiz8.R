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
