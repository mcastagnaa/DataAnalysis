#REFS
#[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
#[2] Uwe F Mayer post on thread https://class.coursera.org/dataanalysis-002/forum/thread?thread_id=1237
# "How to deal with duplicate column names in samsungData"
#[3] Uwe F Mayer post on thread https://class.coursera.org/dataanalysis-002/forum/thread?thread_id=1198
# "Assignment 2: some pointers on getting started

library(MASS)
library(ggplot2)

# dataURL <- "https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda"
dataFile <- "./downloads/samsungData.rda"
# download.file(dataURL, dataFile)
# datadownloaded <- date()
# print(datadownloaded)
# rm(SamData, dataURL)


load(dataFile)
rm(dataFile)

source("Ass2_renameDupCols.R")
names(samsungData) <- names(fixDupCols(samsungData, verbose = FALSE))
samsungData$activity <- as.factor(samsungData$activity)
samsungData[, 1:561] <- scale(samsungData[, 1:561])

#names(samsungData)
#str(samsungData)
table(samsungData$subject)
table(samsungData$activity)

fixTrain <- c(1, 3, 5, 6)
fixTest <- c(27, 28, 29, 30)

otherSubj <- unique(samsungData[! samsungData$subject %in% 
                           c(fixTrain, fixTest),
                         "subject"])
set.seed(1234)
index <- 1:length(otherSubj)
trainindex <- sample(index, ceiling(length(index)/2))

subjTrain <- c(fixTrain, otherSubj[trainindex])
subjTest <- c(fixTest, otherSubj[-trainindex])

samTrain <- na.omit(samsungData[samsungData$subject %in% subjTrain, ])
samTest <- na.omit(samsungData[samsungData$subject %in% subjTest, ])

rm(list= ls(pattern="other."))
rm(list= ls(pattern="fix."))
rm(list= ls(pattern="subj."))
rm(trainindex, index)

table(samTrain$activity, exclude= NULL )


######## BETTER DONE ELSEWHERE

# statsByAct <- t(apply(samTrain[ , 1:561],2, function(col) tapply(col, INDEX =samTrain$activity, FUN=mean)))
# 
# statsStDev <- apply(statsByAct, 1, sd)
# statsMin <- apply(statsByAct, 1, min)
# statsMax <- apply(statsByAct, 1, max)
# statsRange <- statsMax-statsMin
# statsRangeOr <- order(statsRange, decreasing=TRUE)
# statsRange <- statsRange[statsRangeOr]
# rm(statsRangeOr, statsMin, statsMax)
# 
# 
# statsStDev <- apply(statsByAct[, c("walk", "walkdown", "walkup")], 1, sd)
# 
# 
# statsStDev <- apply(statsByAct, 1, sd)
# statsStDevOr <- order(statsStDev, decreasing=TRUE)
# statsStDev <- statsStDev[statsStDevOr]
# 
# lm.formula <- as.formula(paste0("as.numeric(activity) ~ ", 
#         paste(names(head(statsStDev, 50)), collapse="+")))
# 
# rm(statsStDev, statsStDevOr, statsByAct)
# 
# #for(i=1:rows(names(head(statsStDev)))){paste}
# table(as.numeric(samTrain$activity))
# 
# modelTest <- lm(lm.formula, data = samTrain)
# summary(modelTest)
# 
# model <- lm(as.numeric(activity) ~ 
#              fBodyAccJerk.entropy.X +
# #             fBodyAccJerk.entropy.Y +
# #             tBodyAccJerkMag.entropy + 
# #             fBodyAcc.entropy.X + 
#              fBodyBodyAccJerkMag.entropy + 
#              fBodyAccMag.entropy + 
#              tGravityAcc.energy.X + 
#               tGravityAcc.energy.Y + 
#               tGravityAcc.energy.Z + 
# #             fBodyAcc.entropy.Y +
# #             tBodyGyroJerkMag.entropy + 
# #             fBodyAccJerk.entropy.Z + 
#              tBodyAccJerk.entropy.X + 
# #             tBodyAccJerk.entropy.Y + 
#              fBodyBodyGyroJerkMag.entropy + 
# #             tBodyAccMag.entropy  
# #             tGravityAccMag.entropy
#              tBodyAcc.max.X +
#               tBodyAccJerk.max.X +
#               fBodyAcc.bandsEnergy.X.1.8 +
#               fBodyAccMag.mean +
# #              fBodyAccMag.sma
#               tGravityAcc.energy.X #+ 
# #              fBodyAcc.entropy.X + 
# #              tBodyAccJerkMag.entropy           
#       , data = samTrain)
# 
# summary(model)
# 
# #summary(step(lm(as.numeric(samTrain$activity) ~ .
# #             , data = samTrain[, 1:561])))
# 
# predicted <- round(predict(modelTest, samTrain))
# predicted <- replace(predicted, predicted==0,1)
# predicted <- replace(predicted, predicted>6,6)
# actual <- as.numeric(samTrain$activity)
# 
# RMSD <- sqrt(sum((actual-predicted)^2)/length(actual))
# rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
# 
# print(rsq)
# print(RMSD)
# print(RMSD/mean(actual))
# print(paste0("Prediction error: ", as.character(round(RMSD/(max(actual)-min(actual)),3)*100),"%"))
# #http://en.wikipedia.org/wiki/Root-mean-square_deviation
# 
# #png("IntRatePrediction.png", width=480, height=400, units= "px")
# plot(jitter(actual), jitter(predicted), 
#      main = "Activity: actual vs. predicted (train)", cex.main = 1, 
#      yaxt = "n", xaxt = "n", 
#      cex.axis=0.75,
#      bty= "n", ylab = "predicted",
#      xlab = "actual", 
#      col=rgb(0,100,0,30,maxColorValue=255), pch=16)
# axis(1, at=actual
#      , lab=samTrain$activity
#      , las = TRUE, cex.axis=0.75)
# axis(2, at=actual
#      , lab=samTrain$activity
#      , las = FALSE, cex.axis=0.75)
# abline(0,1, col="red")
# #dev.off()
# 
# #using the test data
# predicted <- round(predict(modelTest, samTest))
# predicted <- replace(predicted, predicted==0,1)
# predicted <- replace(predicted, predicted>6,6)
# actual <- as.numeric(samTest$activity)
# RMSD <- sqrt(sum((actual-predicted)^2)/length(actual))
# 
# rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
# print(rsq)
# print(RMSD)
# print(RMSD/mean(actual))
# print(paste0("Prediction error: ", as.character(round(RMSD/(max(actual)-min(actual)),3)*100),"%"))
# #http://en.wikipedia.org/wiki/Root-mean-square_deviation
# 
# #png("IntRatePrediction.png", width=480, height=400, units= "px")
# plot(jitter(actual), jitter(predicted), 
#      main = "Activity: actual vs. predicted (test)", cex.main = 1, 
#      cex.axis=0.75, yaxt = "n", xaxt = "n",
#      bty= "n", ylab = "predicted",
#      xlab = "actual", 
#      col=rgb(0,100,0,30,maxColorValue=255), pch=16)
# axis(1, at=actual
#      , lab=samTest$activity
#      , las = TRUE, cex.axis=0.75)
# axis(2, at=actual
#      , lab=samTest$activity
#      , las = FALSE, cex.axis=0.75)
# abline(0,1, col="red")
# 
# rm(actual, model, predicted, rsq, RMSD)
