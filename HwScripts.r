####All scripts

#week 1

wrkdir<-'/home/brian/Projects/Mit2'
#wrkdir<-"I:\\My Data Sources\\mooc\\Mit2" #at campus
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


wine<-read.csv('./data/wine.csv')
wine_test<-read.csv('./data/wine_test.csv')
str(wine)
summary(wine)
model1<-lm(Price~AGST, data=wine)
summary(model1)

model2<-lm(Price~AGST+HarvestRain, data=wine)
summary(model2)

model3<-lm(Price~.-Year, data=wine)
summary(model3)

#questions with vids
model<-lm(Price~WinterRain+HarvestRain, data=wine)
summary(model)
cor(wine)



climate<-read.csv('./data/climate_change.csv')
str(climate)
train<-subset(climate,Year<=2006)
test<-subset(climate,Year>2006)
model1<-lm(Temp~.-Year -Month, data=train)
summary(model1)
cor(train)

model2<-lm(Temp~MEI+TSI+Aerosols+N2O, data=train)
summary(model2)

Model1Step<-step(model1)

pred1<-predict(Model1Step,newdata=test)

SSE= sum((test$Temp-pred1)^2)
SST= sum((test$Temp-mean(train$Temp))^2)
1-SSE/SST


#week 2 part 2

pisaTrain<-read.csv('./data/pisa2009train.csv')
pisaTest<-read.csv('./data/pisa2009test.csv')

str(pisaTrain)
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore<-lm(readingScore~.,data=pisaTrain)
summary(lmScore)

pred1<-predict(lmScore,newdata=pisaTrain)
SSE= sum((pisaTrain$readingScore-pred1)^2)
RMSE=sqrt(SSE/nrow(pisaTrain))

predTest<-predict(lmScore,newdata=pisaTest)
SSE= sum((pisaTest$readingScore-predTest)^2)
SST= sum((pisaTest$readingScore-mean(pisaTrain$readingScore))^2)

RMSE=sqrt(SSE/nrow(pisaTest))

1-SSE/SST


#week 2 set 3

##FluTrain

FluTrain<-read.csv("./data/FluTrain.csv")
str(FluTrain)
head(FluTrain$Week)
FluTrain$Week<-as.Date(FluTrain$Week)
table1<-tapply(FluTrain$ILI,FluTrain$Week,sum)
which.max(subset(table1,FluTrain$Week<"2010-01-01"))

table2<-tapply(FluTrain$Queries,FluTrain$Week,sum)
table2<-(subset(table2,FluTrain$Week<"2010-01-01"))
which.max(table2)
FluTrain[FluTrain$Week=="2010-10-17",]

hist(FluTrain$ILI)
plot(log(FluTrain$ILI),FluTrain$Que)

FluTrend1<-lm(log(ILI)~Queries,data=FluTrain)
summary(FluTrend1)

cor(log(FluTrain$ILI),FluTrain$Que)^2

FluTest<-read.csv("./data/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
PredTest1[grep("2012-03-11",FluTest$Week)] #finds record of march 11, 2012 in fluTest, then pulls our prediction
#Observed ILI compared to estimated:
(FluTest$ILI[11]-PredTest1[11])/FluTest$ILI[11] #relative error

#week 3 logistic

quality<-read.csv(file="./Data/quality.csv")
install.packages("caTools")

library(caTools)

set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

QualityLog<-glm(PoorCare~StartedOnCombination+ProviderCount,data=qualityTrain,family=binomial)
QualityLog<-glm(PoorCare~OfficeVisits+Narcotics,data=qualityTrain,family=binomial)
predictTest = predict(QualityLog, type="response", newdata=qualityTest)
install.packages("ROCR")
library(ROCR)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
ROCRperf<-performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)




CSD<-read.csv(file="./data/framingham.csv")
str(CSD)


framingham <-CSD

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model
framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)  #table entries are y, x respectively.

# Accuracy
(1069+11)/(1069+6+187+11)

# Baseline accuracy
(1069+6)/(1069+6+187+11)

# Test set AUC
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf<-performance(ROCRpred,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))

#week 3


songs<-read.csv('./data/songs.csv')
str(songs)
(songs[which(songs$artistname=="Michael Jackson" & songs$Top10==1),c('year',"songtitle")])
table(songs$timesignature)
songs[which.max(songs$tempo),2]

SongsTrain<-subset(songs,songs$year<=2009)
SongsTest<-subset(songs,songs$year>2009)
nrow(SongsTrain)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1<-glm(Top10~., data=SongsTrain,family="binomial")
summary(SongsLog1)

cor(SongsTrain$energy,SongsTrain$loudness)


SongsLog2<-glm(Top10~.-loudness, data=SongsTrain,family="binomial")
summary(SongsLog2)

SongsLog3<-glm(Top10~.-energy, data=SongsTrain,family="binomial")
summary(SongsLog3)

songsPred1<-predict(SongsLog3,type='response')
#Train set
tbl<-table(SongsTrain$Top10,songsPred1 > .45)
#accuracy
(tbl[1,1]+tbl[2,2])/(sum(tbl[1,1],tbl[2,1],tbl[1,2],tbl[2,2]))

# Logistic Regression Model
SongsLog3<-glm(Top10~.-energy, data=SongsTrain,family="binomial")
summary(SongsLog3)

# Predictions on the test set
songsPred1<-predict(SongsLog3, type="response", newdata=SongsTest)

# Confusion matrix with threshold of 0.45
tbl<-table(SongsTest$Top10,songsPred1 >.45)

#accuracy
(tbl[1,1]+tbl[2,2])/(sum(tbl[1,1],tbl[2,1],tbl[1,2],tbl[2,2]))

#sensitivity
(tbl[2,2])/(sum(tbl[2,1],tbl[2,2]))

#specificity
(tbl[1,1])/(sum(tbl[1,1],tbl[1,2]))



parole<-read.csv('./data/parole.csv')
table(parole$vio)
parole$state<-as.factor(parole$state)
parole$crime<-as.factor(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

paroleLog<-glm(violator~., data=train,family="binomial")
summary(paroleLog)

"""
Explanation

For parolees A and B who are identical other than A having committed multiple offenses, the predicted log odds of A is 1.61 more than the predicted log odds of B. Then we have:

ln(odds of A) = ln(odds of B) + 1.61

exp(ln(odds of A)) = exp(ln(odds of B) + 1.61)

exp(ln(odds of A)) = exp(ln(odds of B)) * exp(1.61)

odds of A = exp(1.61) * odds of B

odds of A= 5.01 * odds of B

In the second step we raised e to the power of both sides. In the third step we used the exponentiation rule that e^(a+b) = e^a * e^b. In the fourth step we used the rule that e^(ln(x)) = x.
"""

paroleLog$coef
case1<-data.frame(1,1,50,1,3,12,0,2,0)
colnames(case1)<-colnames(parole)
case1$state<-as.factor(case1$state)
case1$crime<-as.factor(case1$crime)
prob<-predict(paroleLog,newdata=case1,type="response")
prob/(1-prob)#odds


#test
predTest<-predict(paroleLog,newdata=test,type="response")
which.max(predTest)

tbl<-table(test$violator,predTest >.5)

#accuracy
(tbl[1,1]+tbl[2,2])/(sum(tbl[1,1],tbl[2,1],tbl[1,2],tbl[2,2]))
#sensitivity
(tbl[2,2])/(sum(tbl[2,1],tbl[2,2]))
#specificity
(tbl[1,1])/(sum(tbl[1,1],tbl[1,2]))
#basic
table(test$violator)

ROCRpredVio = prediction(predTest, test$violator)
auc = as.numeric(performance(ROCRpredVio, "auc")@y.values)
plot(ROCRpredVio,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))


#Prob 3, week 3
loans<-read.csv('./data/loans.csv')
summary(loans)
str(loans)
table(loans$not.fully.paid)
=======

#Week 4 1




gb<-read.csv('./data/gerber.csv')
str(gb)
tbl<-table(gb$voting)
tbl[2]/(tbl[1]+tbl[2])  #proportion voting
tx<-list("hawthorne","civicduty",'neighbors','self','control')

temp<-function(x,y){table(x,y)[4]/sum(table(x,y)[3],table(x,y)[4])}  # note the ordered positions in brackets
lapply(gb[,4:8],function(y) temp(x=gb$voting,y))

gbLog<-glm(voting~civicduty +hawthorne+self+neighbors,data=gb,family='binomial')
summary(gbLog)

pred1<-predict(gbLog,type="response")
tbl<-table(gb$voting,pred1>.3) # works

#accuracy
(tbl[1,1]+tbl[2,2])/(sum(tbl[1,1],tbl[2,1],tbl[1,2],tbl[2,2]))
TableAccuracy<-function(x,y){(table(x,y)[1]+table(x,y)[4])/sum(table(x,y)[1],table(x,y)[2],table(x,y)[3],table(x,y)[4])}

#sensitivity
(tbl[2,2])/(sum(tbl[2,1],tbl[2,2]))
#specificity
(tbl[1,1])/(sum(tbl[1,1],tbl[1,2]))

tbl<-table(gb$voting,pred1>.5) # more complicated because there aren't any above .5.
#fill out the confusion matrix with 0's
 235388/(235388+108696)

library(ROCR)
library(rpart)
library(rpart.plot)

ROCRpredict<-prediction(pred1,gb$voting)
ROCRperf<-performance(ROCRpredict, "tpr","fpr")
auc = as.numeric(performance(ROCRpredict, "auc")@y.values)
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gb)
summary(CARTmodel)
prp(CARTmodel)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gb, cp=0.0)
summary(CARTmodel2)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors  +sex, data=gb, cp=0.0)
summary(CARTmodel3)
prp(CARTmodel3)

CARTmodel4 = rpart(voting ~ +control, data=gb, cp=0.0)
summary(CARTmodel4)
prp(CARTmodel4,digits = 6)

CARTmodel5 = rpart(voting ~ +control +sex, data=gb, cp=0.0)
summary(CARTmodel5)
prp(CARTmodel5,digits = 6)

gbLog1<-glm(voting ~ +control+sex, data=gb, family='binomial')
summary(gbLog1)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(gbLog1, newdata=Possibilities, type="response")

gbLog2<-glm(voting ~ +control*sex, data=gb, family='binomial')
summary(gbLog2)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gb, family="binomial")
summary(LogModel2 )

predict(LogModel2, newdata=Possibilities, type="response")


# VIDEO 4

# Read in the data
stevens = read.csv("./data/stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7)
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree)

# Make predictions
PredictCART = predict(StevensTree, newdata = Test, type = "class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

auc = as.numeric(performance(pred, "auc")@y.values)

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree)
install.packages('randomForest')
library(randomForest)

Train$Reverse<-as.factor(Train$Reverse)
Test$Reverse<-as.factor(Test$Reverse)

set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize=25, ntrees=200)

stevensForPred<-predict(StevensForest,newdata=Test)
table(Test$Reverse,stevensForPred)


cartGrid = expand.grid( .cp = seq(0.01,0.5,0.01))
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds
set.seed(2)
train(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

stevensTreeCV<-rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp=.18)

predictCvTree<-predict(stevensTreeCV,newdata=Test,type='class')
table(Test$Reverse,predictCvTree)

prp(stevensTreeCV)

letters<-read.csv('./data/letters_ABPR.csv')
str(letters)
letters$isB<-as.factor(letters$letter=="B")
library(ROCR)
library(rpart)
library(rpart.plot)

library(caTools)
set.seed(1000)
split<-sample.split(letters$isB,SplitRatio = .5)
train<-subset(letters,split==T)
test<-subset(letters,split==F)

#baseline model, which is just the most frequent score.

table(train$isB)[1]/nrow(train)

CARTb = rpart(isB ~ . - letter, data=train, method="class")

predCartB<-predict(CARTb,newdata=test,type='class')
tbl<-table(test$isB,predCartB)

(tbl[1]+tbl[4])/sum(tbl[1:4])

set.seed(1000)
lettersForest<-randomForest(isB ~ . - letter, data=train)
letterForPred<-predict(lettersForest,newdata=test)
table(test$isB,letterForPred)
(385+393+387+374)/(385+9+374+393+3+7+387)

letters$letter = as.factor( letters$letter )

set.seed(2000)
split<-sample.split(letters$letter,SplitRatio = .5)
train<-subset(letters,split==T)
test<-subset(letters,split==F)

isBCart<-rpart(letter~. -isB,data=train,method= 'class')
predisBCart<-predict(isBCart,newdata=test,type='class')
table(test$letter,predisBCart)

isBForest<-randomForest(letter~. -isB,data=train,type= 'class')
predisBCart<-predict(isBForest,newdata=test,type='class')
table(test$letter,predisBCart)

install.packages("caret")
install.packages("e1071")
library(caret)
library(e1071)


hw prob#3
library(ROCR)
library(rpart)
library(rpart.plot)
library(caTools)

census<-read.csv('./data/census.csv')
str(census)

set.seed(2000)
split<-sample.split(census$over50k,SplitRatio = .6)
train<-subset(census,split==T)
test<-subset(census,split==F)

over50log<-glm(over50k~.,data=train, family='binomial')
summary(over50log)

over50PredLog<-predict(over50log,newdata=test,type='response')
table(test$over50k,over50PredLog >.5)
table(test$over50k)

library(ROCR)

ROCRpredTest = prediction(over50PredLog, test$over50k)
ROCRperf<-performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

over50Cart<-rpart(over50k~.,data=train, method='class')
summary(over50Cart)
rpart.plot(over50Cart)

over50CartPred<-predict(over50Cart,newdata=test,type='prob')
table(test$over50k,over50CartPred[,2]>=.5)

ROCRpredTest= prediction(over50CartPred[,2], test$over50k)
ROCRperf<-performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)


set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
over50Forest<-randomForest(over50k~.,data=trainSmall, method='class')
summary(over50Forest)

over50ForestPred<-predict(over50Forest,newdata=test,type='prob')
table(test$over50k,over50ForestPred[,2]>=.5)

vu = varUsed(over50Forest, count=TRUE,by.tree=F)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(over50Forest$forest$xlevels[vusorted$ix]))

varImpPlot(over50Forest)
set.seed(2)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
fitControl = trainControl( method = "cv", number = 10 ) #cv=cross val, 10= folds
set.seed(2)
train(over50k~.,data=train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

over50Cart<-rpart(over50k~.,data=train, method='class',cp=.002)
rpart.plot(over50Cart)
over50CartPred<-predict(over50Cart,newdata=test)
table(test$over50k,over50CartPred[,2]>.5)

ROCRpredTest= prediction(over50CartPred[,2], test$over50k)
ROCRperf<-performance(ROCRpredTest,"tpr","fpr")
plot(ROCRperf,colorize=T,print.cutoffs.at=seq(0,1,.1),text.adj=c(-.2,1.7))
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)





# Read in the data
Claims = read.csv("./data/ClaimsData.csv")

str(Claims)

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims)

# Split the data
library(caTools)

set.seed(88)

spl = sample.split(Claims$bucket2009, SplitRatio = 0.6)

ClaimsTrain = subset(Claims, spl==TRUE)

ClaimsTest = subset(Claims, spl==FALSE)


# VIDEO 7

# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)

# Penalty Matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

PenaltyMatrix

# Penalty Error of Baseline Method
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest) #penalty error


# VIDEO 8

# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)

prp(ClaimsTree)


# Make predictions
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)

# Penalty Error
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)

# New CART model with loss matrix
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))

# Redo predictions and penalty error
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest)

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)



