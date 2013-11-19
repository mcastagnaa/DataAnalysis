# [1] "Requested"        "Funded"           "InterestRate"     "LoanLength"      
# [5] "LoanPurpose"      "DebtToIncome"     "State"            "HomeOwnership"   
# [9] "MonthlyIncome"    "FICORange"        "OpenLines"        "RevolvingBalance"
# [13] "Inquiries"        "EmploymentLength" "EmpLengthNum"     "LoanLengthFac"   
# [17] "HomeOwnershipFac" "LoanPurposeFac"   "FICORangeFac"     "StateFac"        
# [21] "BalToIncome"      "FundedPerc"       "FICOmid"          "LoanPurposeFreq" 


# http://gettinggeneticsdone.blogspot.co.uk/2011/02/split-data-frame-into-testing-and.html


options(show.signif.stars = F)
completeSet <- rawdata[complete.cases(rawdata),]

set.seed(12345)
index <- 1:nrow(completeSet)
trainindex <- sample(index, trunc(length(index)/2))
trainset <- completeSet[trainindex, ]
testset <- completeSet[-trainindex, ]

str(trainset)
str(testset)

model1 <- lm(InterestRate ~ 
               FICOmid 
             + LoanLengthFac
             + OpenLines
             + Funded
             + Inquiries
             + MonthlyIncome
             , data = trainset)

predicted <- predict(model1, testset)

actual <- testset$InterestRate
RMSD <- sqrt(sum((actual-predicted)^2)/length(actual))

rsq <- 1-sum((actual-predicted)^2)/sum((actual-mean(actual))^2)
print(rsq)
print(RMSD)
print(RMSD/mean(actual))
print(paste0("Prediction error: ", as.character(round(RMSD/(max(actual)-min(actual)),3)*100),"%"))
#http://en.wikipedia.org/wiki/Root-mean-square_deviation

png("IntRatePrediction.png", width=480, height=400, units= "px")
plot(actual, predicted, 
     main = "Interest rate: actual vs. predicted", cex.main = 1, 
     yaxt = "n", xaxt = "n", cex.axis=0.75,
     xlim=c(0, 0.28), ylim=c(0, 0.28), bty= "n")
axis(2, at=pretty(predicted)
     , lab=paste0(pretty(predicted)*100, "%")
     , las = TRUE, cex.axis=0.75)
axis(1, at=pretty(actual)
     , lab=paste0(pretty(actual)*100, "%")
     , las = TRUE, cex.axis=0.75)

abline(0,1, col="red")
dev.off()

