####All scripts

#week 1

wrkdir<-'/home/brian/Projects/Mit2'

setwd(wrkdir)
mvt<-read.csv('./data/mvtWeek1.csv')
str(mvt)
max(mvt$ID)
min(mvt$Beat)
(table((mvt$Month),mvt$Arrest))
table(mvt$LocationDescription=="ALLEY")
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
median(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
sort(table(mvt$Month))
sort(table(mvt$Weekday))
hist(mvt$Date, breaks=100)
boxplot(mvt$Date~mvt$Arrest)
table(mvt$Year==2001,mvt$Arrest)
sort(table(mvt$LocationDescription))

Top5<-subset(mvt, LocationDescription=="STREET"|LocationDescription=="ALLEY"|LocationDescription=='GAS STATION'|LocationDescription=='DRIVEWAY - RESIDENTIAL'|LocationDescription=='PARKING LOT/GARAGE(NON.RESID.)')
sort(table(Top5$LocationDescription))
nrow(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)

tbl<-table(Top5$Loc,Top5$Arrest)
#The following prints the proportions of arrests (tbl[i,2]) to thefts (sum(tbl[i,])) by location (for loop)
for(i in 1:nrow(tbl)){
  print(paste(rownames(tbl)[i],"  ",tbl[i,2]/sum(tbl[i,])))
}

table(Top5$Weekday,Top5$Loc=="GAS STATION")
table(Top5$Weekday,Top5$Loc=="DRIVEWAY - RESIDENTIAL")
    #Week 2