#library(lubridate)
#library(ggplot2)
#library(mice)

#Load data
school<-"I:/My Data Sources/mooc/Mit2"
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
train$train_set<-TRUE
test$Party<-NA
test$train_set<-FALSE
length(test)
df<-rbind(train,test)
summary(df)

#Explore, clean, create variables
df$sum_na<-apply(df,1,function(x) sum(is.na(x))) #adds count variable of NA

df$EducationLevel<-relevel(df$EducationLevel,ref="Current K-12")
df$HouseholdStatus<-relevel(df$HouseholdStatus,ref="Single (no kids)")

df$Income<-relevel(df$Income,ref="over $150,000")            
df$Income<-relevel(df$Income,ref="$100,001 - $150,000")      
df$Income<-relevel(df$Income,ref="$75,000 - $100,000")      
df$Income<-relevel(df$Income,ref="$50,000 - $74,999")     
df$Income<-relevel(df$Income,ref="$25,001 - $50,000")     
df$Income<-relevel(df$Income,ref="under $25,000")       


"""
df$Income.int<-relevel(df$Income,ref="over $150,000")            150
df$Income.int<-relevel(df$Income,ref="$100,001 - $150,000")      100
df$Income<-relevel(df$Income,ref="$75,000 - $100,000")      75
df$Income<-relevel(df$Income,ref="$50,000 - $74,999")      50
df$Income<-relevel(df$Income,ref="$25,001 - $50,000")     25
df$Income<-relevel(df$Income,ref="under $25,000")       0


 Associate's Degree
Bachelor's Degree
Current K-12      
Current Undergraduate
Doctoral Degree
High School Diploma
 Master's Degree 
"""

#convert YOB to date

df$YOB<-year(as.POSIXct(paste(df$YOB,"-01","-01",sep=""),format='%Y'))
df$age<-year(today())-df$YOB #'today()' is a lubrdiate function
p<-ggplot(data=df,aes(x=YOB))
p+geom_bar(aes(fill=Party))+geom_vline(xintercept=2010)
#so, there are outliers and impossible birthdates.  

#the folliwing are cases with odd birth years:
odd_dates<-df[which(df$YOB>2000 | df$YOB< 1925),]
plot(x=odd_dates$age,y=odd_dates$YOB,las=2,na.rm=F,cex=.1)
points(odd_dates$age,y=odd_dates$YOB,col=odd_dates$Party,cex=.5)

for(i in 1:length(df$YOB)){
  if (!is.na(df$YOB[i])){
    if(df$YOB[i] < 1925 | df$YOB[i]>2003) {  #originally 1925 and 2003, respectively; makes some gender issues of zero
      df$YOB[i]<-NA
      print(df$YOB[i])
    }
  }
}

tapply(df$YOB,list(df$EducationLevel,df$Income),mean, na.rm=T)

tapply(df$YOB,list(df$EducationLevel,df$Income),function(x) sum(is.na(x)))

#remove all variables except YOB, education, income to impute YOB

dfParty<-df$Party
dfUser_id<-df$USER_ID
impute_df<-df[,c(-1,-108)]
#at this point, df is still  whole, but needs 'age'
imputed = complete(mice(impute_df))





df$age<-year(today())-df$YOB #'today()' is a lubrdiate function





