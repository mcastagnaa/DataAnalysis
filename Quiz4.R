setwd("G:/GitHub/DataAnalysis")

#Question 1
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt",
              destfile = "movies.txt")
moviesData <- read.table("movies.txt",sep="\t",header=T, quote="")
str(moviesData)

#quiz1
model1 <- lm(score ~ box.office, data = moviesData)
print(model1)
#quiz2
confint(model1, level = 0.9)
#quiz3
model2 <- lm(score ~ box.office + running.time, data = moviesData)
print(model2)
#quiz4
corrlData <- as.matrix(moviesData[, c("box.office", "running.time", "score")])
cor(corrlData)
cor.test(moviesData$box.office, moviesData$running.time)
cor.test(moviesData$score, moviesData$running.time)

#quiz5, 6
plot(moviesData$running.time, moviesData$score)
noOutlier <- moviesData[moviesData$running.time < 200,]
model3 <- lm(score ~ box.office + running.time, data = noOutlier)
summary(model2)
summary(model3)

#quiz 7, 8
model3 <- lm(score ~ rating + running.time + rating*running.time, data = moviesData)
summary(model3)

#quiz 9-10
data(warpbreaks)
str(warpbreaks)

releveled <- within(warpbreaks, tension <- relevel(tension, ref = "M"))

confint