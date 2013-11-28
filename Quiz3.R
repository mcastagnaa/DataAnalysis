setwd("G:/Coursera/DataAnalysis")

#Question 3
library(datasets)
data(iris)
irisSubset <- iris[, 1:4]
hClustering <- hclust(dist(irisSubset))
plot(hClustering)
abline(h=3)
rm(list = c(ls(pattern = "iris"), "hClustering"))

#Question 4

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.rda",
              destfile = "quiz3question4.rda")
load("quiz3question4.rda")
kmeansObj <- kmeans(dataSet, centers = 2)
plot(dataSet$x, dataSet$y, col= kmeansObj$cluster)
rm("dataSet", "kmeansObj")

#Question 5
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(zip.train)
#im <- zip2image(zip.train,3)
#image(im)
par(mfrow = c(2,2))
im8 <- zip2image(zip.train,8)
image(im8, main = "row 8")
im18 <- zip2image(zip.train,18)
image(im18, main = "row 18")

svd8 <- svd(im8)
svd18 <- svd(im18)
plot(svd8$d^2/sum(svd8$d^2), pch=19, xlab="Singular vector", ylab="Variance explained")
plot(svd18$d^2/sum(svd18$d^2), pch=19, xlab="Singular vector", ylab="Variance explained")

print(as.character((svd8$d^2/sum(svd8$d^2))*100))
print(as.character((svd18$d^2/sum(svd18$d^2))*100))

par(mfcol = c(1,1))

rm(list= c(ls(pattern = "im*"), "zip.train", ls(pattern="svd*")))
