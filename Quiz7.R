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
library(tree)
data(Cars93,package="MASS")
set.seed(7363)
library(ipred)
#names(Cars93)
newdata <- data.frame(Type = "Large",Price = 20)
ll <- matrix(NA, nrow = 3, ncol = 1)
for(i in 1:3){
ss <- sample(1:dim(Cars93)[1], replace = T)
model <- tree(DriveTrain ~ Price + Type, data = Cars93)
ll[i] <- predict(model, newdata = newdata, type="class" )
}
ll

#5
library(ElemStatLearn)
library(e1071)
library(randomForest)
data(vowel.train)
data(vowel.test)
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)
set.seed(33833)
rf <- randomForest(y ~., data = vowel.train)
#rf
pred_rf <- predict(rf, vowel.test, type="response")
#rmse(vowel.train$y, pred_rf)
#pred_rf
svm <- svm(y ~ ., data = vowel.train)
pred_svm <- predict(svm, vowel.test, type="response")

#vowel.train$y
finalV <- cbind(vowel.test$y, pred_rf, pred_svm)
#finalV
sum(finalV[,1] != finalV[,2])/dim(finalV)[1]
sum(finalV[,1] != finalV[,3])/dim(finalV)[1]
finalV_same <- finalV[finalV[,3]==finalV[,2], ]
#finalV_same
sum(finalV_same[,1] != finalV_same[,2])/dim(finalV_same)[1]