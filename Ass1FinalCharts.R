library(ggplot2)
library(scales)

#table(rawdata$LoanPurpose)

#TODO: 

pdf("SeparateChart.pdf", paper= "a4r", width = 11)

ggplot(rawdata[rawdata$LoanPurposeFreq > 30,], aes(x=FICOmid, y=InterestRate, color=LoanLengthFac)) + 
  geom_point(shape=1) + 
  geom_smooth(method= "lm", se = TRUE) +
  facet_wrap( ~ LoanPurpose, nrow=2) + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Interest rate vs. FICO by Purpose and maturity \n (for cases with more than 30 observations)") +
  #theme(legend.title=element_blank()) +
  scale_color_discrete(name="Loan Length") +
  scale_y_continuous(labels=percent) +
  ylab("Interest rate") +
  xlab("FICO") 

dev.off()
  
#REF http://www.cookbook-r.com/Graphs/
  
png("latticeRegr.png", width = 600, height = 360, units = "px")
library(lattice)
xyplot(InterestRate ~ FICOmid | LoanLengthFac, 
       data=rawdata, #type=c("p","r"),
       panel = function(x,y,...){
         panel.xyplot(x, y, pch = 21,col = "black")
         panel.lmline(x,y,col = "red")}
)
dev.off()
rawdata[rawdata$InterestRate < 0.07 & rawdata$FICOmid < 720 & rawdata$LoanLengthFac == "60 months", ]