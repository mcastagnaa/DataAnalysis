#Data Analysis Assignment no.1
#Due date 17/11
# https://class.coursera.org/dataanalysis-002/human_grading/view/courses/971332/assessments/4/submissions
# https://class.coursera.org/dataanalysis-002/human_grading/index

# WORK 
setwd("G:/GitHub/DataAnalysis")
#HOME
#setwd("G:/GitHub/DataAnalysis")

dataUrl <- "https://spark-public.s3.amazonaws.com/dataanalysis/loansData.csv"
dataFile <- "./downloads/loansData.csv"
download.file(dataUrl, dataFile)
datadownloaded <- date()
print(datadownloaded)
rawdata <- read.csv(dataFile, as.is = TRUE)

#head(rawdata)

#names transformations
names(rawdata) <- gsub("\\.","",names(rawdata))
names(rawdata) <- gsub("Ratio","",names(rawdata))
names(rawdata) <- gsub("ByInvestors","",names(rawdata))
names(rawdata) <- gsub("intheLast6Months","",names(rawdata))
names(rawdata) <- gsub("CREDIT","",names(rawdata))
names(rawdata) <- gsub("Amount","",names(rawdata))
# [1] "Requested"        "Funded"           "InterestRate"     "LoanLength"      
# [5] "LoanPurpose"      "DebtToIncome"     "State"            "HomeOwnership"   
# [9] "MonthlyIncome"    "FICORange"        "OpenLines"        "RevolvingBalance"
# [13] "Inquiries"        "EmploymentLength"


#dataset structure
head(rawdata)
str(rawdata)

#When NAs occour
dim(rawdata)
sum(complete.cases(rawdata))
rawdata[!complete.cases(rawdata), ]

#first pass
table(rawdata$State)
table(rawdata$FICORange)
table(rawdata$LoanLength)
range(rawdata$Requested)
range(rawdata$Funded)

# FICO ranges from 300 to 850, the higher the better. Median was 711 (2011)
# "According to a Fitch study, the accuracy of FICO in predicting delinquency 
# has diminished in recent years. 
# In 2001 there was an average 31-point difference in the FICO score between 
# borrowers who had defaulted and those who paid on time. 
# By 2006 the difference was only 10 points."
# (http://en.wikipedia.org/wiki/Credit_score_in_the_United_States)

# Data cleaning
rawdata()

# Added variables
FundRequest <-  rawdata$Funded/rawdata$Requested
