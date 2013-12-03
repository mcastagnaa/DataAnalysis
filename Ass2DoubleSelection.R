# statsByAct <- t(apply(scale(samTrain[ , 1:561], scale = TRUE, center=TRUE),
#                       2 , 
#                       function(col) tapply(col, 
#                                            INDEX =samTrain$activity, 
#                                            FUN=mean)))

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
VariablesNoStep3 <- 40
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

varNamesArr <- c(varNamesArr1,varNamesArr2,varNamesArr3 )
print(paste("Duplicate variables:", sum(duplicated(varNamesArr))))

varNames <- paste(varNamesSet1, varNamesSet2, varNamesSet3, sep = "+")


lm.formula <- as.formula(paste0("as.numeric(activity) ~ ",varNames)) 
rm(statsStDevOr, statsByAct)
rm(list= ls(pattern="varName."))


modelTest <- lm(lm.formula, data = samTrain)
rm(lm.formula, statsStDev)

modelStep <- step(modelTest, trace = 0)
summary(modelStep)
rm(modelTest)

predicted <- round(predict(modelStep, samTrain))
predicted <- replace(predicted, predicted==0,1)
predicted <- replace(predicted, predicted>6,6)
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

#png("TrainPrediction.png", width=640, height=640, units= "px")
plot(jitter(actual), jitter(predicted), 
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

#png("TestPrediction.png", width=640, height=640, units= "px")
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
#dev.off()

rm(actual, IdOk, predicted, rsq, RMSD)
