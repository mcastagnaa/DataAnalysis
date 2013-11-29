number.vars <- ncol(samTrain) - 2
digits <- nchar(as.character(number.vars))
pad <- ""; for(i in 1:digits) pad = paste(pad, "0", sep="")
pb <- txtProgressBar(style=3)

for (i in 1:number.vars){
  number=paste(pad, i, sep="");
  number = substring(number, nchar(number)-digits+1)
  filename = paste("Ass2plots/",number,".chart.png", sep="")
  #png(filename)
  par(mfrow=c(1,2))
  hist(samTrain[,i], main=paste(number,names(samTrain)[i]),
       xlab=paste("samTrain[,",i,"]",sep=""))
  
  boxplot(samTrain[,i] ~ samTrain$activity, 
          main=paste(number,names(samTrain)[i]),
          las= 3)
  #dev.off()
  setTxtProgressBar(pb, i/number.vars)
}
par(mfrow=c(1,1))
close(pb)
