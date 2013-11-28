# 1
data(warpbreaks)
names(warpbreaks)
anova1<-aov(breaks ~ wool + tension, data= warpbreaks)
summary(anova1)

# 3
install.packages("glm2")
library(glm2)
data(crabs)
names(crabs)
glm1 <- glm(Satellites ~ Width, data = crabs, family = "poisson")
summary(glm1)
exp(glm1$coeff[2])

#4
exp(glm1$coeff[1] + glm1$coeff[2] * 22)

#5
install.packages("MASS")
library(MASS)
data(quine) 
lm1 = lm(log(Days + 2.5) ~.,data=quine)
step(lm1)