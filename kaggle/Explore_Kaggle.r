##Kaggle competition 2016, May
#

# KAGGLE COMPETITION - GETTING STARTED
#Load data
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))
dater<-getwd()
setwd(paste0(dater, "/Mit2"))
dir()

train = read.csv("./kaggle/train2016.csv",stringsAsFactors=F)#,na.strings = c("NA", '',' '))
test = read.csv("./kaggle/test2016.csv",stringsAsFactors=F)#,na.strings = c("NA", '',' '))

#Explore and clean
#train<-train[complete.cases(train)==T,]  # read.csv na.strings = '' complete cases = 697

#convert blanks "" into "blank"; must read.csv stringsAsFactors=F
train[train=='']<-'blank'
test[test=='']<-'blank'
#convert to factor
train[,3:108]<-lapply(train[,3:108],factor)
test[,3:108]<-lapply(test[,3:108],factor)

apply(train,2,(table))
boxplot(train$YOB)
summary(train$YOB)
par(mfrow=c(1,1))
boxplot(train$age~train$Party)

table(train$Gender,train$Party,is.na(train$age))


#convert YOB to date
library(lubridate)
train$YOB<-year(as.POSIXct(paste(train$YOB,"-01","-01",sep=""),format='%Y'))
test$YOB<-year(as.POSIXct(paste(test$YOB,"-01","-01",sep=""),format='%Y'))
#create age variable
train$age<-year(today())-train$YOB #'today()' is a lubrdiate function
test$age<-year(today())-test$YOB #'today()' is a lubrdiate function

boxplot(train$age)
boxplot(test$age)

train[which(train$age>80),c('age',"Gender","Income","HouseholdStatus","EducationLevel","Party")]
test[which(test$age>80),c('age',"Gender","Income","HouseholdStatus","EducationLevel")]




# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:

SimpleMod = glm(Party ~ . -USER_ID -YOB, data=train, family=binomial)
summary(SimpleMod)
# And then make predictions on the test set:
PredTrain = predict(SimpleMod,newdata=train, type="response")
PredTest = predict(SimpleMod, newdata=test, type="response")

threshold = 0.5
PredTrainLabels = as.factor(ifelse(PredTrain<threshold, "Democrat", "Republican"))
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

tbl<-table(train$Party,PredTrain>.50)
tbl
(tbl[1]+tbl[4])/sum(tbl)
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "./kaggle/SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!


#cart

library(rpart)
library(rpart.plot)

simpleCartMod1<-rpart(Party ~ . -USER_ID -YOB, data=train, method="class")
prp(simpleCartMod1)

predCartTrain1<-predict(simpleCartMod1,newdata=train,type='class')
tbl<-table(train$Party,predCartTrain1)
tbl
(tbl[1]+tbl[4])/sum(tbl)


predCart1<-predict(simpleCartMod1,newdata=test,type='class')

MySubmissionCart = data.frame(USER_ID = test$USER_ID, Predictions = predCart1)

write.csv(MySubmissionCart, "./kaggle/SubmissionSimpleCart.csv", row.names=FALSE)

