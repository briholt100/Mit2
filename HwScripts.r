####All scripts

#week 1

wrkdir<-'/home/brian/Projects/Mit2'

setwd(wrkdir)
mvt<-read.csv('./data/mvtWeek1.csv')
str(mvt)
max(mvt$ID)
min(mvt$Beat)
table(mvt$LocationDescription=="ALLEY")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)





#Week 2