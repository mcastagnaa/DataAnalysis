# [1] "Requested"        "Funded"           "InterestRate"     "LoanLength"      
# [5] "LoanPurpose"      "DebtToIncome"     "State"            "HomeOwnership"   
# [9] "MonthlyIncome"    "FICORange"        "OpenLines"        "RevolvingBalance"
# [13] "Inquiries"        "EmploymentLength" "EmpLengthNum"     "LoanLengthFac"   
# [17] "HomeOwnershipFac" "LoanPurposeFac"   "FICORangeFac"     "StateFac"        
# [21] "BalToIncome"      "FundedPerc"       "FICOmid"

library(ellipse)

options(show.signif.stars = F)
model1 <- lm(InterestRate ~ 
               FICOmid 
             + DebtToIncome
             + LoanLengthFac
             + OpenLines
             + Funded
             + BalToIncome
             + HomeOwnershipFac
             + LoanPurposeFac
             + StateFac
             + EmpLengthNum
             + Inquiries
             + MonthlyIncome
             , data = rawdata[complete.cases(rawdata),])

model2 <- update(model1, .~.- BalToIncome - StateFac- LoanPurposeFac)
model3 <- update(model2, .~.- HomeOwnershipFac)
model4 <- update(model3, .~.- DebtToIncome)
model5 <- update(model4, .~.- EmpLengthNum)
model6 <- update(model5, .~. + LoanPurposeFac)
modelaov <- aov(InterestRate ~ LoanPurposeFac , data = rawdata[complete.cases(rawdata),])

modelScaled <- lm(scale(InterestRate) ~ 
                          scale(FICOmid) 
                  + scale(as.numeric(sub(" months", "", LoanLengthFac)))
                  + scale(OpenLines)
                  + scale(Funded)
                  + scale(Inquiries)
                  + scale(MonthlyIncome)
                  , data = rawdata[complete.cases(rawdata),])

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(modelScaled)
summary(model6)

anova(model1, model2)
anova(model2, model3)
anova(model3, model4)
anova(model4, model5)
anova(modelaov)


#try this
#step(model1, direction="backward")
#step(modelScaled, direction="both")

confint(model5)
#par(mfrow=c(2,2))
#plot(model5)
#par(mfrow=c(1,1))

####http://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html
#####http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplelinear.html

#correlation

rawdata$LoanLengthMonths <- as.numeric(sub(" months", "", rawdata$LoanLengthFac))
correlationSet <- rawdata[complete.cases(rawdata), c("DebtToIncome",
                                                     "LoanLengthMonths",
                                                     "OpenLines",
                                                     "Funded",
                                                     "BalToIncome",
                                                     "EmpLengthNum",
                                                     "Inquiries",
                                                    "MonthlyIncome")]

corrSet.matrix <- as.matrix(correlationSet)
ctab <- cor(corrSet.matrix)
print(ctab)

png("correlation.png")
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(ctab, col=rgb(colorfun((ctab+1)/2), 
                       maxColorValue=255), 
                      cex.lab = 0.7,
              main = "Correlation analysis for dependent variables")
mtext("the less spheric the higher the correlation \n (blue indicates positive correlation, red negative)",
      side = 1, line = 3, cex = 0.7)
dev.off()
#cleanup
rm(list = ls(pattern = "model."))

   
