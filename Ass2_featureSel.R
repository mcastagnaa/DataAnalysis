#Dimension reduction
svd1 <- svd(samTrain[, 1:561])
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)

par(mfrow=c(1,2))
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
par(mfrow=c(1,1))

pca1 <- prcomp(samTrain[, 1:561])
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singular Vector 1")
abline(c(0,1))