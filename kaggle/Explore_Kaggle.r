##Kaggle competition 2016, May
#

#preprocess
#impute
#logistic
#cart
#randomForrest



# KAGGLE COMPETITION - GETTING STARTED
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

#expore
#Explore, clean, create variables
df$sum_na<-apply(df,1,function(x) sum(is.na(x))) #adds count variable of NA

"""
df$Income.int<-relevel(df$Income,ref="over $150,000")            150
df$Income.int<-relevel(df$Income,ref="$100,001 - $150,000")      100
df$Income<-relevel(df$Income,ref="$75,000 - $100,000")      75
df$Income<-relevel(df$Income,ref="$50,000 - $74,999")      50
df$Income<-relevel(df$Income,ref="$25,001 - $50,000")     25
df$Income<-relevel(df$Income,ref="under $25,000")       0
"""




#convert YOB to date
#library(lubridate)
df$YOB<-year(as.POSIXct(paste(df$YOB,"-01","-01",sep=""),format='%Y'))

boxplot(df$YOB)

plot(df$YOB,df$sum_na,cex=.3)
line<-(lm(df$sum_na~df$YOB))
abline(line,col="purple",lwd=3)
points(y=df$sum_na[df$Party=='Democrat'],x=df$YOB[df$Party=='Democrat'],col="blue")
points(y=df$sum_na[df$Party=='Republican'],x=df$YOB[df$Party=='Republican'],col="red")
points(y=df$sum_na[is.na(df$Party)],x=df$YOB[is.na(df$Party)],col="green")

tail(sort(df$YOB),20)

summary(df$YOB)
plot(x=df$USER_ID,df$YOB,pch=19, col='blue',cex=.1)
abline(h=2000,col='green')
abline(h=1925,col='red')  #originally 1925
points(x=df$USER_ID[df$YOB>='2003'],y=df$YOB[df$YOB>='2003'],col="black",cex=.3,pch=3)
points(x=df$USER_ID[df$YOB<'1925'],y=df$YOB[df$YOB<'1925'],col="green",cex=.3,pch=3)
points(x=df$USER_ID[is.na(df$YOB)==T],y=df$YOB[is.na(df$YOB)==T],col="orange",cex=.3,pch=3)

for(i in 1:length(df$YOB)){
  if (!is.na(df$YOB[i])){
    if(df$YOB[i] < 1925 | df$YOB[i]>2003) {  #originally 1925 and 2003, respectively; makes some gender issues of zero
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

# loop converts to na YOB outside realistic birth years
for(i in 1:length(df$YOB)){
  if (!is.na(df$YOB[i])){
    if(df$YOB[i] < 1925 | df$YOB[i]>2003) {  #originally 1925 and 2003, respectively; makes some gender issues of zero
      df$YOB[i]<-NA
      print(df$YOB[i])
    }
  }
}

tapply(df$YOB,list(df$EducationLevel,df$Income),mean, na.rm=T)  #mean yob by income and education
tapply(df$YOB,list(df$EducationLevel,df$Income),function(x) sum(is.na(x))) #count of YOB na by income, education
median_yob_tbl<-tapply(df$YOB,list(df$EducationLevel,df$Income),median, na.rm=T)#median yob by income and education

# What follows takes median_yob_tbl above and uses row and colnames to find NA in YOB
df[which(df$EducationLevel == rownames(median_yob_tbl)[6] & df$Income == colnames(median_yob_tbl)[1]  & is.na(df$YOB)==T),]
#Income has 6 levels
#EducationLevel has 7 levels.
#for (j in 1:length(colnames(median_yob_tbl))){
#  for (i in 1:length(rownames(median_yob_tbl))){
#    print(rownames(median_yob_tbl)[i]);  print(colnames(median_yob_tbl)[j])
#    }
#}

for(i in which(is.na(df$YOB)==T)){yob_na<-which(is.na(df$YOB)==T)}  #creates list of records with YOB ==NA

# The following goes and replaces df$YOB with NA's the median year of birth, but only for full records in income and education
count<-0
for(i in 1:nrow(df[yob_na,])){
  for (j in 1:length(colnames(median_yob_tbl))){
    for (k in 1:length(rownames(median_yob_tbl))){
      if(!is.na(as.logical(colnames(median_yob_tbl)[j] == df$Income[i] &
                             rownames(median_yob_tbl)[k]==df$EducationLevel[i]))){
        df$YOB[yob_na][i]<-(median_yob_tbl[k,j])}
    }
  }
  count<-count+1
}
print (count)

table(is.na(df$YOB))
df[is.na(df$YOB),1:6]

for(i in which(is.na(df$YOB)==T)){yob_na<-which(is.na(df$YOB)==T)}  #creates list of records with YOB ==NA

#The following replaces NA with Median of EducationLEvel
count<-0
for(i in 1:nrow(df[yob_na,])){
  for (j in 1:length(rownames(median_yob_tbl))){
    if(!is.na(as.logical(rownames(median_yob_tbl)[j]==df$EducationLevel[i]))){
      df$YOB[yob_na][i]<-apply(median_yob_tbl,1,median)[j]}
  }
  count<-count+1
}
print (count)

#for some reason record 9 won't update.  They are in K-12, which has a median 1997
median(df$YOB[df$EducationLevel == "Current K-12"],na.rm=T)
df[9,'YOB']<-1997
table(is.na(df$YOB))

#############################################
#############################################
#       After Imputing
#       Correlations
#
#############################################
#############################################


df_matrix<-data.matrix(df)

correlations <- rcorr(df_matrix[,-1])
objects(correlations)
correlations$P
which(abs(correlations$P)<.001,arr.ind=T)
attr(correlations$P,which='dimnames')[[2]][11]

library(corrgram)
corrgram(df_matrix[,2:20])


temp1<-cor(df_matrix[,-1],df_matrix[,-1])
corrgram(temp1)

cor.df<-as.data.frame(cbind(which(abs(temp1)>.2 & abs(temp1)<.99,arr.ind=T),
                            temp1[which(abs(temp1)>.2 & abs(temp1)<.99,arr.ind=F)]),row.names=T)

cor.df$rowName<-rownames(which(abs(temp1)>.2 & abs(temp1)<.99,arr.ind=T))
cor.df$colName<-colnames(temp1)[cor.df$col]
colnames(cor.df)<-c('temp1.row.pos','temp1.col.pos','r','rowName','colName')

cor.df$r<-as.numeric(as.character(cor.df$r))
head(cor.df[order(abs(cor.df$r),decreasing=T),],10)
duplicated(cor.df$r)


#############################################
#############################################
#       After imputing
#
#############################################
#############################################

#imputed_df<-read.csv("./kaggle/imputed_df.csv")
df<-imputed_df[,-1]


#############################################
#############################################
#       Imputing
#
#############################################
#############################################


#install.packages("mice")
#library(mice)

#remove party from imputation
dfParty<-df$Party
dfUser_id<-df$USER_ID
impute_df<-df[,c(-1,-108)]
#at this point, df is still  whole, but needs 'age'
imputed = complete(mice(impute_df))

df$Party<-dfParty
df$USER_ID<-dfUser_id
imputed_df<-imputed

#create age variable
library(lubridate)
imputed_df$age<-year(today())-imputed_df$YOB #'today()' is a lubrdiate function
imputed_df$USER_ID<-df$USER_ID
imputed_df$Party<-df$Party
summary(imputed_df)
write.csv(imputed_df, "./kaggle/imputed_df.csv")

df<-imputed_df

#return df back to test and train, split train set

train<-df[which(df$train_set==T),]
test<-df[which(df$train_set==F),]
nrow(test)
nrow(train)

set.seed(144)
spl = sample(1:nrow(train), size=0.7*nrow(train))
train_df = train[spl,]
test_df = train[-spl,]
