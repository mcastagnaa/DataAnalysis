library(randomForest)
statsByAct <- t(apply(samTrain[ , 1:561], 2,
                      function(col) tapply(col, 
                                           INDEX =samTrain$activity, 
                                           FUN=mean)))

statsByAct <- statsByAct[! duplicated(statsByAct),]
statsStDev <- apply(statsByAct, 1, sd)
statsStDevOr <- order(statsStDev, decreasing=TRUE)
statsStDev <- statsStDev[statsStDevOr]

VariablesNoStep1 <- 5
VariablesNoStep2 <- 10
VariablesNoStep3 <- 10
#VariablesNoStep4 <- 1

varNamesArr1 <- names(head(statsStDev, VariablesNoStep1))
varNamesSet1 <- paste(names(head(statsStDev, VariablesNoStep1)), collapse="+")

statsStDev <- apply(statsByAct[,1:3], 1, sd)
statsStDevOr <- order(statsStDev, decreasing=TRUE)
statsStDev <- statsStDev[statsStDevOr]
varNamesArr2 <- names(head(statsStDev, VariablesNoStep2))
varNamesSet2 <- paste(names(head(statsStDev, VariablesNoStep2)), collapse="+")

statsStDev <- apply(statsByAct[,4:6], 1, sd)
statsStDevOr <- order(statsStDev, decreasing=TRUE)
statsStDev <- statsStDev[statsStDevOr]
varNamesArr3 <- names(head(statsStDev, VariablesNoStep3))
varNamesSet3 <- paste(names(head(statsStDev, VariablesNoStep3)), collapse="+")

#statsStDev <- apply(statsByAct[,c(4,6)], 1, sd)
#statsStDevOr <- order(statsStDev, decreasing=TRUE)
#statsStDev <- statsStDev[statsStDevOr]
#varNamesArr4 <- names(head(statsStDev, VariablesNoStep4))
#varNamesSet4 <- paste(names(head(statsStDev, VariablesNoStep4)), collapse="+")


varNamesArr <- c(varNamesArr1,varNamesArr2,varNamesArr3)#, varNamesArr4 )
print(paste("Duplicate variables:", sum(duplicated(varNamesArr))))

varNames <- paste(varNamesSet1, varNamesSet2, varNamesSet3, sep = "+")
#, varNamesSet4)

lm.formula <- as.formula(paste0("activity ~ ",varNames)) 
rm(statsStDevOr, statsByAct)
rm(list= ls(pattern="varName."))

modelTest <- randomForest(lm.formula, data = samTrain)
modelTest

plot(modelTest)

predicted <- predict(modelTest, samTrain, type="class")
actual <- samTrain$activity

table(actual, predicted)

rm(lm.formula, statsStDev)

predictedNo <- as.numeric(predicted)
actualNo <- as.numeric(samTrain$activity)

RMSD <- sqrt(sum((actualNo-predictedNo)^2)/length(actualNo))
rsq <- 1-sum((actualNo-predictedNo)^2)/sum((actualNo-mean(actualNo))^2)
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
predicted <- predict(modelTest, samTest, type="class")
actual <- samTest$activity

table(actual, predicted)

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

#png("TestFinalTree.png", width=640, height=640, units= "px")
plot(jitter(actualNo), jitter(predictedNo), 
     main = "Activity: test set - random forest model", cex.main = 1, 
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
