startingVariablesSet <- 150

set.seed(123)
index <- 1:561
trainindex <- sample(index, startingVariablesSet)


lm.formula <- as.formula(paste0("as.numeric(activity) ~ ", 
        paste(names(samsungData[, trainindex]), collapse="+")))
rm(trainindex, index)

modelTest <- lm(lm.formula, data = samTrain)
rm(lm.formula)

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

png("TrainPrediction_rnd.png", width=640, height=640, units= "px")
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

#png("TestPrediction_rnd.png", width=640, height=640, units= "px")
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
