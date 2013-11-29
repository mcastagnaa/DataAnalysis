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
names(samsungData) <- names(fixDupCols(samsungData))
samsungData$activity <- as.factor(samsungData$activity)

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
otherTrain <- otherSubj[trainindex]
otherTest <- otherSubj[-trainindex]

subjTrain <- c(fixTrain, otherTrain)
subjTest <- c(fixTest, otherTest)

samTrain <- na.omit(samsungData[samsungData$subject %in% subjTrain, ])
samTest <- na.omit(samsungData[samsungData$subject %in% subjTest, ])

rm(list= ls(patter="other."))
rm(list= ls(patter="fix."))
rm(list= ls(patter="subj."))
rm(trainindex, index, samsungData)

glm1 <- glm(activity ~ ., data = samTrain, family = "gaussian")

summary(samTrain[,c(317, 331)])

table(samTrain$activity, exclude= NULL )

