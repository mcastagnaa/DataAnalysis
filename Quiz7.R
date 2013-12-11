rmse <- function(actual, predicted) {
  return(sqrt(sum((actual-predicted)^2)/length(actual)))
}
#2
library(splines)
set.seed(53535)
xValues = seq(0,2*pi,length=100)
yValues = rnorm(100) + sin(xValues)

for(i in 1:10) {
  ns1 <- ns(xValues, df = i)
  lmsp <- lm(yValues ~ ns1)
  print(paste0(i, ": ", rmse(yValues, lmsp$fitted)))
}

#3
library(simpleboot)
data(airquality)
attach(airquality)
names(airquality)
quantile(airquality$Wind, 0.75)
set.seed(883833)
getQntl <- function(x) {quantile(x, 0.75)}
qntlBoot <- one.boot(airquality$Wind, getQntl, R=1000)
sd(qntlBoot$t)

# set.seed(883833)
# getQntl <- function(x,i) {quantile(x[i], 0.75)}
# qntlBoot <- boot(airquality$Wind, getQntl, R=1000)
# qntlBoot

#4
data(Cars93,package="MASS")
set.seed(7363)
library(ipred)
bagTree <- bagging()