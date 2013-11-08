# [1] "Requested"        "Funded"           "InterestRate"     "LoanLength"      
# [5] "LoanPurpose"      "DebtToIncome"     "State"            "HomeOwnership"   
# [9] "MonthlyIncome"    "FICORange"        "OpenLines"        "RevolvingBalance"
# [13] "Inquiries"        "EmploymentLength" "EmpLengthNum"     "LoanLengthFac"   
# [17] "HomeOwnershipFac" "LoanPurposeFac"   "FICORangeFac"     "StateFac"        
# [21] "BalToIncome"      "FundedPerc"       "FICOmid"


options(show.signif.stars = F)
model1 <- lm(InterestRate ~ 
               FICOmid 
             + DebtToIncome
             + LoanLengthFac
             + OpenLines
             + BalToIncome
             + HomeOwnershipFac
             + LoanPurposeFac
             + StateFac
             + EmpLengthNum
             + Inquiries
             + MonthlyIncome
             , data = rawdata)

model2 <- update(model1, .~.- BalToIncome - StateFac - LoanPurposeFac)
model3 <- update(model2, .~.- HomeOwnershipFac)
model4 <- update(model3, .~.- OpenLines)
model5 <- update(model4, .~.- DebtToIncome)

summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)


anova(model1, model2)
anova(model2, model3)
anova(model3, model4)
anova(model4, model5)

#try this
step(model1, direction="backward")

confint(model5)
par(mfrow=c(2,2))
plot(model5)
par(mfrow=c(1,1))
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/multregr.html
#http://ww2.coastal.edu/kingw/statistics/R-tutorials/simplelinear.html

#TO DO: correlation and prediction

#cleanup
rm(list = ls(pattern = "model."))

   
