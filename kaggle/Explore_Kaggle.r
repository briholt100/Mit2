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

#join test and train
train<-kaggle_train
test<-kaggle_test
test$Party<-NA
trainParty<-train$Party
train<-train[,-7]  #step 1 to move 'party' to end
train$Party<-trainParty #step 2 to move party to end
train$data_set<-"train"
test$Party<-NA
test$data_set<-"test"
length(test)
df<-rbind(train,test)


#Explore, clean, create variables
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

tail(sort(df$YOB),20)

summary(df$YOB)
plot(x=df$USER_ID,df$YOB,pch=19, col='blue',cex=.1)
abline(h=2000,col='green')
abline(h=1925,col='red')
points(x=df$USER_ID[df$YOB>='2003'],y=df$YOB[df$YOB>='2003'],col="black",cex=.3,pch=3)
points(x=df$USER_ID[df$YOB<'1925'],y=df$YOB[df$YOB<'1925'],col="green",cex=.3,pch=3)
points(x=df$USER_ID[is.na(df$YOB)==T],y=df$YOB[is.na(df$YOB)==T],col="orange",cex=.3,pch=3)

for(i in 1:length(df$YOB)){
  if (!is.na(df$YOB[i])){
    if(df$YOB[i] < 1925 | df$YOB[i]>2003) {
      df$YOB[i]<-NA
      print(df$YOB[i])
    }
  }
}


#relevel
#######for df
df$Income<-relevel(df$Income,ref="over $150,000")
df$Income<-relevel(df$Income,ref="$100,001 - $150,000")
df$Income<-relevel(df$Income,ref="$75,000 - $100,000")
df$Income<-relevel(df$Income,ref="$50,000 - $74,999")
df$Income<-relevel(df$Income,ref="$25,001 - $50,000")
df$Income<-relevel(df$Income,ref="under $25,000")

levels(df$EducationLevel)
df$EducationLevel<-relevel(df$EducationLevel,ref="Current K-12")
df$HouseholdStatus<-relevel(df$HouseholdStatus,ref="Single (no kids)")






#return df back to test and train, split train set

train<-df[which(df$data_set=='train'),]
test<-df[which(df$data_set!='train'),]
nrow(test)

set.seed(144)
spl = sample(1:nrow(train), size=0.7*nrow(train))
train_df = train[spl,]
test_df = train[-spl,]





#df<-df[complete.cases(df)==T,]  # read.csv na.strings = '' complete cases = 697

install.packages("mice")
library(mice)








# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:

SimpleMod = glm(Party ~ . -USER_ID -YOB, data=train_df, family=binomial)
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




# Multiple imputation for all variables, including test set, with no preprocessing other than adding 'age' and sum_na.
#set.seed(144)


summary(df)
imputed = complete(mice(df[,2:110]))
summary(imputed)
imputed$USER_ID <-df$USER_ID
summary(imputed)
write.csv(imputed, "./kaggle/Full_imputed.csv", row.names=FALSE)

temp<-merge(test,imputed,by.x="USER_ID",by.y="USER_ID",all.x=T)

MySubmissionFull_Imput <- data.frame(USER_ID = test$USER_ID, Predictions = temp$Party.y)

write.csv(MySubmissionFull_Imput, "./kaggle/MySubmissionFull_Imput.csv", row.names=FALSE)

#this does not work well.