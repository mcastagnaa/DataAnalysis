#$ Requested       : int  20000 19200 35000 10000 12000 6000 10000 33500 14675 7000 ...
#$ Funded          : num  20000 19200 35000 9975 12000 ...
#$ InterestRate    : num  0.089 0.1212 0.2198 0.0999 0.1171 ...
#$ LoanLength      : chr  "36 months" "36 months" "60 months" "36 months" ...
#$ LoanPurpose     : chr  "debt_consolidation" "debt_consolidation" "debt_consolidation" "debt_consolidation" ...
#$ DebtToIncome    : num  0.149 0.284 0.238 0.143 0.188 ...
#$ State           : chr  "SC" "TX" "CA" "KS" ...
#$ HomeOwnership   : chr  "MORTGAGE" "MORTGAGE" "MORTGAGE" "MORTGAGE" ...
#$ MonthlyIncome   : num  6542 4583 11500 3833 3195 ...
#$ FICORange       : chr  "735-739" "715-719" "690-694" "695-699" ...
#$ OpenLines       : int  14 12 14 10 11 17 10 12 9 8 ...
#$ RevolvingBalance: int  14272 11140 21977 9346 14469 10391 15957 27874 7246 7612 ...
#$ Inquiries       : int  2 1 1 0 0 2 0 0 1 0 ...
#$ EmploymentLength: chr  "< 1 year" "2 years" "2 years" "5 years" ...
#$ EmpLengthNum    : num  0 2 2 5 9 3 11 11 8 3 ...
#$ LoanLengthFac   : Factor w/ 2 levels "36 months","60 months": 1 1 2 1 1 1 1 2 1 1 ...
#$ HomeOwnershipFac: Factor w/ 4 levels "MORTGAGE","OTHER",..: 1 1 1 1 4 3 4 1 4 4 ...
#$ LoanPurposeFac  : Factor w/ 14 levels "car","credit_card",..: 3 3 3 3 2 10 3 2 2 2 ...
#$ FICORangeFac    : Factor w/ 38 levels "640-644","645-649",..: 20 16 11 12 12 7 17 14 10 16 ...
#$ StateFac        : Factor w/ 46 levels "AK","AL","AR",..: 37 39 5 16 28 7 19 18 5 5 ...
#$ BalToIncome     : num  2.18 2.43 1.91 2.44 4.53 ...
#$ FundedPerc      : num  1 1 1 0.998 1 ...
#$ FICOmid         : num

par(mfrow=c(2,1))
rawdata.temp <- rawdata[rawdata$LoanLengthFac == "36 months", ]
plot(rawdata.temp$FICORangeFac, rawdata.temp$InterestRate, 
     main = rawdata.temp$LoanLengthFac[1], 
     xlab = "FICO", ylab = "Int.rate", yaxt = "n")
axis(2, at=pretty(rawdata.temp$InterestRate)
     , lab=paste0(pretty(rawdata.temp$InterestRate)*100, "%")
     , las = TRUE)
rawdata.temp <- rawdata[rawdata$LoanLengthFac == "60 months", ]
plot(rawdata.temp$FICORangeFac, rawdata.temp$InterestRate, 
     main = rawdata.temp$LoanLengthFac[1], 
     xlab = "FICO", ylab = "Int.rate", yaxt = "n")
axis(2, at=pretty(rawdata.temp$InterestRate)
     , lab=paste0(pretty(rawdata.temp$InterestRate)*100, "%")
     , las = TRUE)
par(mfrow=c(1,1))
rm(rawdata.temp)


par(mfrow=c(2,1))
rawdata.temp <- rawdata[rawdata$LoanLengthFac == "36 months", ]
plot(rawdata.temp$FICOmid, rawdata.temp$InterestRate, 
     main = rawdata.temp$LoanLengthFac[1], 
     xlab = "FICO", ylab = "Int.rate", yaxt = "n")
axis(2, at=pretty(rawdata.temp$InterestRate)
     , lab=paste0(pretty(rawdata.temp$InterestRate)*100, "%")
     , las = TRUE)
abline(lm(rawdata.temp$InterestRate ~ rawdata.temp$FICOmid), col = "red", lwd =2)

rawdata.temp <- rawdata[rawdata$LoanLengthFac == "60 months", ]
plot(rawdata.temp$FICOmid, rawdata.temp$InterestRate, 
     main = rawdata.temp$LoanLengthFac[1], 
     xlab = "FICO", ylab = "Int.rate", yaxt = "n")
axis(2, at=pretty(rawdata.temp$InterestRate)
     , lab=paste0(pretty(rawdata.temp$InterestRate)*100, "%")
     , las = TRUE)
abline(lm(rawdata.temp$InterestRate ~ rawdata.temp$FICOmid), col = "red", lwd =2)
par(mfrow=c(1,1))
rm(rawdata.temp)


plot(rawdata$FICORangeFac, rawdata$FundedPerc, 
     main = "Conceded vs. FICO", 
     xlab = "FICO", ylab = "Funded/Requested", yaxt = "n")
axis(2, at=pretty(rawdata$FundedPerc)
     , lab=paste0(pretty(rawdata$FundedPerc)*100, "%")
     , las = TRUE)

plot(rawdata$DebtToIncome, rawdata$FundedPerc, 
     main = "Conceded vs. DebtToIncome", 
     xlab = "DebtToIncome", ylab = "Funded/Requested"
     , yaxt = "n"
     , xaxt = "n")
axis(2, at=pretty(rawdata$FundedPerc)
     , lab=paste0(pretty(rawdata$FundedPerc)*100, "%")
     , las = TRUE)
axis(1, at=pretty(rawdata$DebtToIncome)
     , lab=paste0(pretty(rawdata$DebtToIncome)*100, "%")
     , las = TRUE)

par(mfrow=c(2,1))
plot(rawdata$Inquiries, rawdata$FundedPerc, 
     main = "Conceded vs. Inquiries", 
     xlab = "Inquiries", ylab = "Funded/Requested"
     , yaxt = "n")
axis(2, at=pretty(rawdata$FundedPerc)
     , lab=paste0(pretty(rawdata$FundedPerc)*100, "%")
     , las = TRUE)
plot(rawdata$Inquiries, rawdata$InterestRate, 
     main = "IntRate vs. Inquiries", 
     xlab = "Inquiries", ylab = "Int.Rate"
     , yaxt = "n")
axis(2, at=pretty(rawdata$InterestRate)
     , lab=paste0(pretty(rawdata$InterestRate)*100, "%")
     , las = TRUE)
par(mfrow=c(1,1))


rawdata.temp <- rawdata[rawdata$LoanLengthFac == "36 months", ]
plot(rawdata.temp$DebtToIncome, rawdata.temp$InterestRate, 
     main = "Int.Rate vs. DebtToIncome (36m)", 
     xlab = "DebtToIncome", ylab = "Int.Rate"
     , yaxt = "n"
     , xaxt = "n")
axis(2, at=pretty(rawdata.temp$InterestRate)
     , lab=paste0(pretty(rawdata.temp$InterestRate)*100, "%")
     , las = TRUE)
axis(1, at=pretty(rawdata.temp$DebtToIncome)
     , lab=paste0(pretty(rawdata.temp$DebtToIncome)*100, "%")
     , las = TRUE)
rm(rawdata.temp)

plot(rawdata$HomeOwnershipFac, rawdata$InterestRate, 
     main = "Int.Rate vs. Home ownership", 
     xlab = "HomeOwnership", ylab = "Int.Rate"
     , yaxt = "n")
axis(2, at=pretty(rawdata$InterestRate)
     , lab=paste0(pretty(rawdata$InterestRate)*100, "%")
     , las = TRUE)

plot(rawdata$StateFac, rawdata$InterestRate, 
     main = "InterestRate vs. State", 
     xlab = "State", ylab = "Int.Rate"
     , yaxt = "n")
axis(2, at=pretty(rawdata$InterestRate)
     , lab=paste0(pretty(rawdata$InterestRate)*100, "%")
     , las = TRUE)

#ordered by median value
par(mar=c(8, 4, 4, 2))
bym <- with(rawdata, reorder(LoanPurposeFac, InterestRate, median))
plot(bym, rawdata$InterestRate
     , main = "InterestRate vs. Loan Purpose"
     , xlab = "", ylab = "Int.Rate"
     , yaxt = "n"
     , xaxt = "n")
axis(2, at=pretty(rawdata$InterestRate)
     , lab=paste0(pretty(rawdata$InterestRate)*100, "%")
     , las = TRUE)
axis(1, las = 2
     , at = bym
     , labels = bym
     , cex.axis = 0.75)
mtext(side = 1, text = "Loan Purpose", line = 7)
par(mar=c(5.1, 4.1, 4.1, 2.1))
rm(bym)

#not ordered by median value
#par(mar=c(8, 4, 4, 2))
#plot(rawdata$LoanPurposeFac, rawdata$InterestRate,
#     , main = "InterestRate vs. Loan Purpose"
#     , xlab = "", ylab = "Int.Rate"
#     , yaxt = "n"
#     , xaxt = "n")
#axis(2, at=pretty(rawdata$InterestRate)
#     , lab=paste0(pretty(rawdata$InterestRate)*100, "%")
#     , las = TRUE)
#axis(1, las = 2
#     , at = rawdata$LoanPurposeFac
#     , labels = rawdata$LoanPurpose
#     , cex.axis = 0.75)
#mtext(side = 1, text = "Loan Purpose", line = 7)
#par(mar=c(5.1, 4.1, 4.1, 2.1))


pairs(formula = ~ FICOmid 
      + InterestRate 
      + DebtToIncome 
      + FundedPerc 
      + BalToIncome 
      + OpenLines 
      + EmpLenghtNum, data = rawdata)


#http://www.statmethods.net/advgraphs/parameters.html