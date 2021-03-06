statsByAct <- t(apply(samTrain[ , 1:561], 2,
                      function(col) tapply(col,
                                           INDEX =samTrain$activity, 
                                           FUN=mean)))

statsByAct <- statsByAct[! duplicated(statsByAct),]
statsStDev <- apply(statsByAct, 1, sd)
statsStDevOr <- order(statsStDev, decreasing=TRUE)
statsStDev <- statsStDev[statsStDevOr]

rm(statsStDevOr, statsByAct)

startingVariablesSet <- 2

#For Random
set.seed(1)
index <- 1:561
trainindex <- sample(index, startingVariablesSet)
lm.formula <- as.formula(paste0("as.numeric(activity) ~ ", 
                                paste(names(samsungData[, trainindex]), 
                                      collapse="+")))
rm(trainindex, index)

# For StDev based
# lm.formula <- as.formula(paste0("as.numeric(samTrain$activity) ~ ", 
#         paste(names(head(statsStDev, startingVariablesSet)), collapse="+")))
# 
# 
modelTest <- lm(lm.formula, data = samTrain)
rm(lm.formula, statsStDev)

#modelStep <- step(modelTest, trace = 0)
modelStep <- modelTest

summary(modelStep)
rm(modelTest)

predicted <- round(predict(modelStep, samTrain))
predicted <- replace(predicted, predicted < 1, 1)
predicted <- replace(predicted, predicted > 6, 6)
actual <- as.numeric(samTrain$activity)

RMSD <- sqrt(sum((actual-predicted)^2)/length(actual))
rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)
print(RMSD)
print(RMSD/mean(actual))
print(paste0("Prediction error: ", as.character(round(RMSD/(max(actual)-min(actual)),3)*100),"%"))
IdOk <- sum(actual==predicted)/length(actual)
print(paste0("Identified: ", as.character(round(IdOk,3)*100), "%"))
#http://en.wikipedia.org/wiki/Root-mean-square_deviation

png("TrainPrediction_2var_rnd.png", width=640, height=640, units= "px")
plot(jitter(actual), jitter(predicted), 
     main = "Activity: actual vs. predicted (train)", cex.main = 1, 
     yaxt = "n", xaxt = "n", 
     cex.axis=0.75,
     bty= "n", ylab = "predicted",
     xlab = "actual", 
     col=rgb(0, 100, 0, 30, maxColorValue=255), pch=16,
     xlim=c(0,6), ylim=c(0,6))
axis(1, at=actual
     , lab=samTrain$activity
     , las = TRUE, cex.axis=0.75)
axis(2, at=actual
     , lab=samTrain$activity
     , las = FALSE, cex.axis=0.75)
abline(0,1, col="red")
dev.off()

#using the test data
predicted <- round(predict(modelStep, samTest))
predicted <- replace(predicted, predicted==0,1)
predicted <- replace(predicted, predicted>6,6)
actual <- as.numeric(samTest$activity)
RMSD <- sqrt(sum((actual-predicted)^2)/length(actual))

rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)
print(RMSD)
print(RMSD/mean(actual))
print(paste0("Prediction error: ", as.character(round(RMSD/(max(actual)-min(actual)),3)*100),"%"))
IdOk <- sum(actual==predicted)/length(actual)
print(paste0("Identified: ", as.character(round(IdOk,3)*100), "%"))
#http://en.wikipedia.org/wiki/Root-mean-square_deviation

png("TestPrediction.png", width=640, height=640, units= "px")
plot(jitter(actual), jitter(predicted), 
     main = "Activity: actual vs. predicted (test)", cex.main = 1, 
     cex.axis=0.75, yaxt = "n", xaxt = "n",
     bty= "n", ylab = "predicted",
     xlab = "actual", 
     col=rgb(0,100,0,30,maxColorValue=255), pch=16)
axis(1, at=actual
     , lab=samTest$activity
     , las = TRUE, cex.axis=0.75)
axis(2, at=actual
     , lab=samTest$activity
     , las = FALSE, cex.axis=0.75)
abline(0,1, col="red")
dev.off()

rm(actual, IdOk, predicted, rsq, RMSD, modelStep)
