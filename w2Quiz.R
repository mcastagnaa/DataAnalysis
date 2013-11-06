#Question2

url <- "http://simplystatistics.tumblr.com/"
con <- url(url, "r")
simplyStats <- readLines(con, n=150)
close(con)
countCharLines <- nchar(simplyStats)
print(countCharLines[c(2, 45, 122)])

#Question3
url <- "https://spark-public.s3.amazonaws.com/dataanalysis/ss06hid.csv"
download.file(url, "./downloads/w2quiz3.csv", method="curl")
con <-file("./downloads/w2quiz3.csv")
USCommData <- read.csv(con)
#3
print(table(USCommData$VAL)[24])
#4
USCommData$FES
#5
table(USCommData$BDS, USCommData$RMS)
#6 ACR = 3, AGS = 6  
agricultureLogical <- USCommData$ACR == 3 & USCommData$AGS == 6 
which(agricultureLogical)
#7
indexes <- which(agricultureLogical)
subsetDataFrame  <- USCommData[indexes,] 
sum(is.na(subsetDataFrame$MRGX))