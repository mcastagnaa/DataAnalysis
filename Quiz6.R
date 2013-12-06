##Week 6 quiz

#3
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

names(trainSA)
glml <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
            data = trainSA, 
            family = "binomial")


missClass <- function(values,prediction){
  sum(((prediction > 0.5)*1) != values)/length(values)
}

missClass(trainSA$chd, predict(glml, type="response"))
missClass(testSA$chd, predict(glml, testSA, type="response"))


#4
library(pgmm)
library(tree)
data(olive)
olive = olive[,-1]

names(olive)
class(olive$Area)
olive$Area <- as.factor(olive$Area)

tree <- tree(Area ~ ., data = olive)
plot(tree)
text(tree, cex = 0.75)

par(mfrow=c(1,2))
plot(cv.tree(tree, FUN=prune.tree, method="misclass"))
plot(cv.tree(tree))
par(mfrow=c(1,1))

pruneTree <- prune.tree(tree, best=6)
plot(pruneTree)
text(pruneTree, cex = 0.75)

newData <- data.frame(Palmitic = 1200, 
                      Palmitoleic = 120, 
                      Stearic=200,
                      Oleic=7000,
                      Linoleic = 900, 
                      Linolenic = 32, 
                      Arachidic=60,
                      Eicosenoic=6)

predict(pruneTree, newData, )


