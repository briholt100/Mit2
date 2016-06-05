##Kaggle competition 2016, May
#

# KAGGLE COMPETITION - GETTING STARTED
#Load data
school<-"I:/My Data Sources/mooc/MitAnalytic"
setwd(school)
home<-getwd()
setwd(paste0(home, "/mooc/MitAnal"))

latitude<-getwd()
setwd(paste0(latitude, "/Projects/Mit2"))

dater<-getwd()
setwd(paste0(dater, "/Mit2"))
dir()

kaggle_train <- read.csv("./kaggle/train2016.csv",stringsAsFactors=T,na.strings = c("NA", '',' '))
kaggle_test <- read.csv("./kaggle/test2016.csv",stringsAsFactors=T,na.strings = c("NA", '',' '))
str(kaggle_train)
str(kaggle_test)

#join test and train
train<-kaggle_train
test<-kaggle_test
trainParty<-train$Party
train<-train[,-7]
train$Party<-trainParty
test$Party<-NA
length(test)
df<-rbind(train,test)


#Explore and clean

df$sum_na<-apply(df,1,function(x) sum(is.na(x))) #adds count variable of NA

#convert YOB to date
library(lubridate)
df$YOB<-year(as.POSIXct(paste(df$YOB,"-01","-01",sep=""),format='%Y'))

#create age variable
df$age<-year(today())-df$YOB #'today()' is a lubrdiate function

boxplot(df$age)

plot(df$age,df$sum_na)
line<-(lm(df$sum_na~df$age))
abline(line,col="purple",lwd=3)
points(y=df$sum_na[df$Party=='Democrat'],x=df$age[df$Party=='Democrat'],col="blue")
points(y=df$sum_na[df$Party=='Republican'],x=df$age[df$Party=='Republican'],col="red")
points(y=df$sum_na[is.na(df$Party)],x=df$age[is.na(df$Party)],col="green")

df[which(df$age>80),c('age',"Gender","Income","HouseholdStatus","EducationLevel","Party")]


#df<-df[complete.cases(df)==T,]  # read.csv na.strings = '' complete cases = 697


install.packages("mice")
library(mice)

# Multiple imputation
#set.seed(144)
summary(df)
imputed = complete(mice(df[,2:110]))
summary(imputed)
df[,2:110] = imputed
summary(df)
write.csv(df, "df.csv", row.names=FALSE)





set.seed(144)
spl = sample(1:nrow(train), size=0.7*nrow(train))
train = train[spl,]
test = train[-spl,]





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

