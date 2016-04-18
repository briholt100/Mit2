####All scripts

#week 1

wrkdir<-'/home/brian/Projects/Mit2'

setwd(wrkdir)

WHO<-read.csv('./data/WHO.csv')
summary(WHO)

sort((WHO$Over60))
WHO$Coun[WHO$Over60<1]
sort((WHO$LiteracyRate))
(WHO[WHO$LiteracyRate>99,c(1,10)])
tapply(WHO$ChildMortality,WHO$Region,mean)


#  Analytic detective
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


#stock market
IBM<-read.csv('./data/IBMStock.csv')
GE<-read.csv('./data/GEStock.csv')
CocaCola<-read.csv('./data/CocaColaStock.csv')
ProcterGamble<-read.csv('./data/ProcterGambleStock.csv')
Boeing<-read.csv('./data/BoeingStock.csv')
str(GE)

# convert date variable from factor

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
stocks<-c('IBM','GE','CocaCola','ProcterGamble','Boeing')

lapply(stocks,function(x){max(x[,2])})
lapply(stocks,function(x){sd(x[[2]])})

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)


plot(x=CocaCola$Date,CocaCola$StockPrice,pch=19,cex=.2, col=4, lty=4)
abline(h=min(CocaCola$StockPrice), col='red')

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col='black',lty=2)
abline(h=min(ProcterGamble$StockPrice), col='green')
#Week 2