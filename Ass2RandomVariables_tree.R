startingVariablesSet <- 25

set.seed(123)
index <- 1:561
trainindex <- sample(index, startingVariablesSet)

lm.formula <- as.formula(paste0("activity ~ ", 
        paste(names(samsungData[, trainindex]), collapse="+")))
rm(trainindex, index)

modelTest <- tree(lm.formula, data = samTrain)

modelStep <- modelTest
summary(modelStep)
rm(modelTest)
plot(modelStep)
text(modelStep, cex=0.5)

par(mfrow=c(1,2))
plot(cv.tree(modelStep, FUN=prune.tree, method="misclass"))
plot(cv.tree(modelStep))
par(mfrow=c(1,1))

pruneTree <- prune.tree(modelStep, best=7)
plot(pruneTree)
text(pruneTree, cex=0.5)

summary(pruneTree)

predicted <- predict(pruneTree, samTrain, type="class")
actual <- samTrain$activity

table(actual, predicted)

rm(lm.formula, statsStDev, modelStep)

predictedNo <- as.numeric(predicted)
actualNo <- as.numeric(samTrain$activity)

RMSD <- sqrt(sum((actualNo-predictedNo)^2)/length(actualNo))
rsq <- 1-sum((actualNo-predictedNo)^2)/sum((actual-mean(actualNo))^2)
print(rsq)
print(RMSD)
print(RMSD/mean(actualNo))
print(paste0("Prediction error: ", as.character(round(RMSD/(max(actualNo)-min(actualNo)),3)*100),"%"))
IdOk <- sum(actualNo==predictedNo)/length(actualNo)
print(paste0("Identified: ", as.character(round(IdOk,3)*100), "%"))
#http://en.wikipedia.org/wiki/Root-mean-square_deviation

#png("TrainPrediction_tripleSel_StDev.png", width=640, height=640, units= "px")
plot(jitter(actualNo), jitter(predictedNo), 
     main = "Activity: actual vs. predicted (train)", cex.main = 1, 
     yaxt = "n", xaxt = "n", 
     cex.axis=0.75,
     bty= "n", ylab = "predicted",
     xlab = "actual", 
     col=rgb(0, 100, 0, 30, maxColorValue=255), pch=16)
axis(1, at=actual
     , lab=samTrain$activity
     , las = TRUE, cex.axis=0.75)
axis(2, at=actual
     , lab=samTrain$activity
     , las = FALSE, cex.axis=0.75)
abline(0,1, col="red")
#dev.off()

#using the test data
predicted <- predict(pruneTree, samTest, type="class")
actual <- samTest$activity
predictedNo <- as.numeric(predicted)
actualNo <- as.numeric(samTest$activity)

RMSD <- sqrt(sum((actualNo-predictedNo)^2)/length(actualNo))

rsq <- 1-sum((actualNo-predictedNo)^2)/sum((actualNo-mean(actualNo))^2)
print(rsq)
print(RMSD)
print(RMSD/mean(actualNo))
print(paste0("Prediction error: ", as.character(round(RMSD/(max(actualNo)-min(actualNo)),3)*100),"%"))
IdOk <- sum(actualNo==predictedNo)/length(actualNo)
print(paste0("Identified: ", as.character(round(IdOk,3)*100), "%"))
#http://en.wikipedia.org/wiki/Root-mean-square_deviation

#png("TestPrediction.png", width=640, height=640, units= "px")
plot(jitter(actualNo), jitter(predictedNo), 
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
#dev.off()

rm(actual, IdOk, predicted, rsq, RMSD, pruneTree, predictedNo, actualNo)
