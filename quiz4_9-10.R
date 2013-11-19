data(warpbreaks)
str(warpbreaks)

releveled <- within(warpbreaks, tension <- relevel(tension, ref = "M"))

confint