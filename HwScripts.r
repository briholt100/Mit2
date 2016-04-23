####All scripts

#week 1

wrkdir<-'/home/brian/Projects/Mit2'
#wrkdir<-"I:\My Data Sources\mooc\Mit2" #at campus
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
stocks<-list(IBM,GE,CocaCola,ProcterGamble,Boeing)
names(stocks) <- c("IBM", "GE", "CocaCola",'Proctor','Boeing' )
lapply(stocks,function(x){max(x[,2])})
lapply(stocks,function(x){sd(x[[2]])})
lapply(stocks,function(x){x[1,2]})



mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)


plot(x=CocaCola$Date,CocaCola$StockPrice,pch=19,cex=.2, col=4, lty=4)
abline(h=min(CocaCola$StockPrice), col='red')

lines(ProcterGamble$Date, ProcterGamble$StockPrice, col='black',lty=2)
abline(h=min(ProcterGamble$StockPrice), col='green')
abline(v=as.Date(c("1983-03-01")), lwd=2,col=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col=3, ylim=c(0,210))

plot_Lines<-function(x,y){
  y<-readline('please enter a number for a color...... ')
  lines(x$Date[301:432],
        x$StockPrice[301:432],
        col=y,
        ylim=c(0,210)
        )
  }
lapply(stocks[c(1:5)],plot_Lines)
legend("topright",legend=names(stocks),text.col=c(1:5), cex=.8)
abline(v=as.Date("1997-09-01"),col='red')
abline(v=as.Date("1997-11-01"),col='blue')

abline(v=as.Date("2004-01-01"),col='red')
abline(v=as.Date("2005-01-01"),col='blue')


sort(tapply(IBM$StockPrice,months(IBM$Date),mean))

lapply(stocks,function(x){sort(tapply(x$StockPrice,months(x$Date),mean))})
lapply(stocks,function(x){mean(x$StockPrice)})

#Week 2

CPS<-read.csv('./data/CPSData.csv')
summary(CPS)
str(CPS)

MetroAreaMap<-read.csv('./data/MetroAreaCodes.csv')
str(MetroAreaMap)
CountryMap<-read.csv('./data/CountryCodes.csv')
str(CountryMap)


sort(table(CPS$Ind,useNA='always'))
sort(table(CPS$State))
sort(table(CPS$Cit))/nrow(CPS)

(table(CPS$Hispanic,CPS$Race))
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Cit, is.na(CPS$Married))
(table(CPS$State, is.na(CPS$MetroA)))

(table(CPS$Region,is.na(CPS$MetroA)))
sort(tapply(is.na(CPS$MetroA),CPS$State,mean))

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS)
str(CPS)
sort(table(CPS$MetroArea))
sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=T))

CPS = merge(CPS, CountryMap , by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

sort(table(CPS$Country))

table(CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA",CPS$Country!="United States",useNA='no')

Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India?


#Extra work
poll<-read.csv('./data/AnonymityPoll.csv')
str(poll)


#Week 2