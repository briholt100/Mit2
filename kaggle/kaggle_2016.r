library(lubridate)
library(ggplot2)
library(mice)
library(caret)
library(Hmisc)
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


#imputed_df<-read.csv("./kaggle/imputed_df.csv")
df<-imputed_df[,-1]
df_matrix<-data.matrix(df)

correlations <- rcorr(df_matrix[,-1])
objects(correlations)
correlations$P
library(corrgram)
corrgram(df_matrix[,2:30])


temp1<-cor(df_matrix[,-1],df_matrix[,2:25])
temp2<-cor(df_matrix[,-1],df_matrix[,26:56])
temp3<-cor(df_matrix[,-1],df_matrix[,57:85])
temp4<-cor(df_matrix[,-1],df_matrix[,86:111])
corrgram(temp1)
corrgram(temp2)
corrgram(temp3)
corrgram(temp4)

apply(t,2,function(x){y<-which(x>.5 & x < .99)
                      if(is.na(y)==F){
                        print(y)
                      }
                      }
      )
#return df back to test and train, split train set

train<-df[which(df$train_set==T),]
test<-df[which(df$train_set==F),]
nrow(test)
nrow(train)

set.seed(144)
spl = sample(1:nrow(train), size=0.7*nrow(train))
train_df = train[spl,]
test_df = train[-spl,]



####
##

#Below is the beginning of clustering
#

df.train.matrix<-data.matrix(train_df)
df.test.matrix<-data.matrix(test_df)
test.matrix<-data.matrix(test)
"""train.df.matrix<-df.matrix[which(df.matrix[,107]==1),]
test.df.matrix<-df.matrix[which(df.matrix[,107]!=1),]
ncol(train.df.matrix)
ncol(test.df.matrix)
dim(test.df.matrix)
"""
# Compute distances
distance = dist(df.train.matrix[,-c(107,109)], method = "euclidean")
heatmap(train.df.matrix[,-c(107,109:110)])
plot(rowMeans(train.df.matrix),,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(train.df.matrix),xlab="Column",ylab="Column Mean",pch=19)

# Hierarchical clustering
distanceTrain = dist(df.train.matrix[,-c(107,109)], method = "euclidean")
distanceTest = dist(df.test.matrix[,-c(107,109)], method = "euclidean")
distanceTest_final = dist(test.matrix[,-c(107,109,110)], method = "euclidean")

clusterIntensity = hclust(distanceTrain, method="ward.D")
plot(clusterIntensity)
k=6
rect.hclust(clusterIntensity, k , border = "blue")

test.clusterIntensity = hclust(distanceTest, method="ward.D")
plot(test.clusterIntensity)
rect.hclust(test.clusterIntensity, k , border = "blue")

#distanceTest_final
test_final.clusterIntensity = hclust(distanceTest_final, method="ward.D")
plot(test_final.clusterIntensity)
rect.hclust(test_final.clusterIntensity, k , border = "blue")


trainClusters = cutree(clusterIntensity, k)
testClusters = cutree(test.clusterIntensity, k)
test_finalClusters = cutree(test_final.clusterIntensity, k)
train_df$trainClusters<-as.factor(trainClusters)
test_df$testClusters<-as.factor(testClusters)
test$testClusters<-as.factor(test_finalClusters)



hclust1.train_df<-subset(train_df,train_df$trainClusters==1)
hclust2.train_df<-subset(train_df,train_df$trainClusters==2)
hclust3.train_df<-subset(train_df,train_df$trainClusters==3)
hclust4.train_df<-subset(train_df,train_df$trainClusters==4)
hclust5.train_df<-subset(train_df,train_df$trainClusters==5)
hclust6.train_df<-subset(train_df,train_df$trainClusters==6)

hclust7.train_df<-subset(train_df,train_df$trainClusters==7)
hclust8.train_df<-subset(train_df,train_df$trainClusters==8)
hclust9.train_df<-subset(train_df,train_df$trainClusters==9)
hclust10.train_df<-subset(train_df,train_df$trainClusters==10)
hclust11.train_df<-subset(train_df,train_df$trainClusters==11)
hclust12.train_df<-subset(train_df,train_df$trainClusters==12)
hclust13.train_df<-subset(train_df,train_df$trainClusters==13)
hclust14.train_df<-subset(train_df,train_df$trainClusters==14)
hclust15.train_df<-subset(train_df,train_df$trainClusters==15)
hclust16.train_df<-subset(train_df,train_df$trainClusters==16)
hclust17.train_df<-subset(train_df,train_df$trainClusters==17)


hclust1.test_df<-subset(test_df,test_df$testClusters==1)
hclust2.test_df<-subset(test_df,test_df$testClusters==2)
hclust3.test_df<-subset(test_df,test_df$testClusters==3)
hclust4.test_df<-subset(test_df,test_df$testClusters==4)
hclust5.test_df<-subset(test_df,test_df$testClusters==5)
hclust6.test_df<-subset(test_df,test_df$testClusters==6)

hclust7.test_df<-subset(test_df,test_df$testClusters==7)
hclust8.test_df<-subset(test_df,test_df$testClusters==8)
hclust9.test_df<-subset(test_df,test_df$testClusters==9)
hclust10.test_df<-subset(test_df,test_df$testClusters==10)
hclust11.test_df<-subset(test_df,test_df$testClusters==11)
hclust12.test_df<-subset(test_df,test_df$testClusters==12)
hclust13.test_df<-subset(test_df,test_df$testClusters==13)
hclust14.test_df<-subset(test_df,test_df$testClusters==14)
hclust15.test_df<-subset(test_df,test_df$testClusters==15)
hclust16.test_df<-subset(test_df,test_df$testClusters==16)
hclust17.test_df<-subset(test_df,test_df$testClusters==17)


#final_test
hclust1.test<-subset(test,test$testClusters==1)
hclust2.test<-subset(test,test$testClusters==2)
hclust3.test<-subset(test,test$testClusters==3)
hclust4.test<-subset(test,test$testClusters==4)
hclust5.test<-subset(test,test$testClusters==5)
hclust6.test<-subset(test,test$testClusters==6)







hcluster.train_df.list<-list(hclust1.train_df,
                   hclust2.train_df,
                   hclust3.train_df,
                   hclust4.train_df,
                   hclust5.train_df,
                   hclust6.train_df,
                   hclust7.train_df,
                   hclust8.train_df,
                   hclust9.train_df,
                   hclust10.train_df,
                   hclust11.train_df,
                   hclust12.train_df,
                   hclust13.train_df,
                   hclust14.train_df,
                   hclust15.train_df,
                   hclust16.train_df,
                   hclust17.train_df
)


###Now, must run rpart, but when I make individual predictions, I must merge the data based on UserID.

#cart

library(rpart)
library(rpart.plot)


#list of models
#Making list of models
hClustCartMod.list<-NULL
for (i in 1:length(hcluster.train_df.list)){
  hClustCartMod.list[i]<-rbind(paste0("hcluster.train_df.model.",i))
}


#populate each model

hcluster.train_df.model.1<-rpart(Party ~ . -YOB , data=hclust1.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.2<-rpart(Party ~ . -YOB , data=hclust2.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.3<-rpart(Party ~ . -YOB , data=hclust3.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.4<-rpart(Party ~ . -YOB , data=hclust4.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.5<-rpart(Party ~ . -YOB , data=hclust5.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.6<-rpart(Party ~ . -YOB , data=hclust6.train_df[,c(-107,-109,-112)], method="class")

hcluster.train_df.model.7<-rpart(Party ~ . -YOB , data=hclust7.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.8<-rpart(Party ~ . -YOB , data=hclust8.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.9<-rpart(Party ~ . -YOB , data=hclust9.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.10<-rpart(Party ~ . -YOB , data=hclust10.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.11<-rpart(Party ~ . -YOB , data=hclust11.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.12<-rpart(Party ~ . -YOB , data=hclust12.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.13<-rpart(Party ~ . -YOB , data=hclust13.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.14<-rpart(Party ~ . -YOB , data=hclust14.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.15<-rpart(Party ~ . -YOB , data=hclust15.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.16<-rpart(Party ~ . -YOB , data=hclust16.train_df[,c(-107,-109,-112)], method="class")
hcluster.train_df.model.17<-rpart(Party ~ . -YOB , data=hclust17.train_df[,c(-107,-109,-112)], method="class")


predhClustCartTrain1<-predict(hcluster.train_df.model.1,newdata=hclust1.test,type='class')
predhClustCartTrain2<-predict(hcluster.train_df.model.2,newdata=hclust2.test,type='class')
predhClustCartTrain3<-predict(hcluster.train_df.model.3,newdata=hclust3.test,type='class')
predhClustCartTrain4<-predict(hcluster.train_df.model.4,newdata=hclust4.test,type='class')
predhClustCartTrain5<-predict(hcluster.train_df.model.5,newdata=hclust5.test,type='class')
predhClustCartTrain6<-predict(hcluster.train_df.model.6,newdata=hclust6.test,type='class')

predhClustCartTrain7<-predict(hcluster.train_df.model.7,newdata=hclust7.test_df,type='class')
predhClustCartTrain8<-predict(hcluster.train_df.model.8,newdata=hclust8.test_df,type='class')
predhClustCartTrain9<-predict(hcluster.train_df.model.9,newdata=hclust9.test_df,type='class')
predhClustCartTrain10<-predict(hcluster.train_df.model.10,newdata=hclust10.test_df,type='class')
predhClustCartTrain11<-predict(hcluster.train_df.model.11,newdata=hclust11.test_df,type='class')
predhClustCartTrain12<-predict(hcluster.train_df.model.12,newdata=hclust12.test_df,type='class')
predhClustCartTrain13<-predict(hcluster.train_df.model.13,newdata=hclust13.test_df,type='class')
predhClustCartTrain14<-predict(hcluster.train_df.model.14,newdata=hclust14.test_df,type='class')
predhClustCartTrain15<-predict(hcluster.train_df.model.15,newdata=hclust15.test_df,type='class')
predhClustCartTrain16<-predict(hcluster.train_df.model.16,newdata=hclust16.test_df,type='class')
predhClustCartTrain17<-predict(hcluster.train_df.model.17,newdata=hclust17.test_df,type='class')


all.predictions<-c(predhClustCartTrain1,
                   predhClustCartTrain2,
                   predhClustCartTrain3,
                   predhClustCartTrain4,
                   predhClustCartTrain5,
                   predhClustCartTrain6,

                   predhClustCartTrain7,
                   predhClustCartTrain8,
                   predhClustCartTrain9,
                   predhClustCartTrain10,
                   predhClustCartTrain11,
                   predhClustCartTrain12,
                   predhClustCartTrain13,
                   predhClustCartTrain14,
                   predhClustCartTrain15,
                   predhClustCartTrain16,
                   predhClustCartTrain17)

predhClustCartTest1<-predict(hcluster.train_df.model.1,newdata=hclust1.test,type='class')
predhClustCartTest2<-predict(hcluster.train_df.model.2,newdata=hclust2.test,type='class')
predhClustCartTest3<-predict(hcluster.train_df.model.3,newdata=hclust3.test,type='class')
predhClustCartTest4<-predict(hcluster.train_df.model.4,newdata=hclust4.test,type='class')
predhClustCartTest5<-predict(hcluster.train_df.model.5,newdata=hclust5.test,type='class')
predhClustCartTest6<-predict(hcluster.train_df.model.6,newdata=hclust6.test,type='class')

all.predictions<-c(predhClustCartTest1,
                   predhClustCartTest2,
                   predhClustCartTest3,
                   predhClustCartTest4,
                   predhClustCartTest5,
                   predhClustCartTest6)





#The follwing should have same length:
length(all.predictions)
nrow(test_df)
test_df$Pred.Party<-NA
test_df[testClusters==1,"Pred.Party"]<-predhClustCartTrain1
test_df[testClusters==2,"Pred.Party"]<-predhClustCartTrain2
test_df[testClusters==3,"Pred.Party"]<-predhClustCartTrain3
test_df[testClusters==4,"Pred.Party"]<-predhClustCartTrain4
test_df[testClusters==5,"Pred.Party"]<-predhClustCartTrain5
test_df[testClusters==6,"Pred.Party"]<-predhClustCartTrain6

test_df[testClusters==7,"Pred.Party"]<-predhClustCartTrain7
test_df[testClusters==8,"Pred.Party"]<-predhClustCartTrain8
test_df[testClusters==9,"Pred.Party"]<-predhClustCartTrain9
test_df[testClusters==10,"Pred.Party"]<-predhClustCartTrain10
test_df[testClusters==11,"Pred.Party"]<-predhClustCartTrain11
test_df[testClusters==12,"Pred.Party"]<-predhClustCartTrain12
test_df[testClusters==13,"Pred.Party"]<-predhClustCartTrain13
test_df[testClusters==14,"Pred.Party"]<-predhClustCartTrain14
test_df[testClusters==15,"Pred.Party"]<-predhClustCartTrain15
test_df[testClusters==16,"Pred.Party"]<-predhClustCartTrain16
test_df[testClusters==17,"Pred.Party"]<-predhClustCartTrain17


tbl<-table(test_df$Party,test_df$Pred.Party)
tbl
(tbl[1]+tbl[4])/sum(tbl)


#The follwing should have same length:
length(all.predictions)
nrow(test)
test$Pred.Party<-NA
test[test_finalClusters==1,"Pred.Party"]<-predhClustCartTest1
test[test_finalClusters==2,"Pred.Party"]<-predhClustCartTest2
test[test_finalClusters==3,"Pred.Party"]<-predhClustCartTest3
test[test_finalClusters==4,"Pred.Party"]<-predhClustCartTest4
test[test_finalClusters==5,"Pred.Party"]<-predhClustCartTest5
test[test_finalClusters==6,"Pred.Party"]<-predhClustCartTest6

test$Pred.Party[test$Pred.Party == 2]<-"Republican"
test$Pred.Party[test$Pred.Party == 1]<-"Democrat"

MySubmissionClusterCart = data.frame(USER_ID = test$USER_ID, Predictions = test$Pred.Party)

write.csv(MySubmissionClusterCart, "./kaggle/SubmissionSimpleClusterCart.csv", row.names=FALSE)





hClustCartMod1<-rpart(Party ~ . -USER_ID -train_set -YOB, data=hclust1.train, method="class")
prp(hClustCartMod1)

predCartTrain1<-predict(simpleCartMod1,newdata=test_df,type='class')
tbl<-table(test_df$Party,predCartTrain1)
tbl
(tbl[1]+tbl[4])/sum(tbl)




###K-means

k=4
set.seed(1000)
KMC = kmeans(trainMatrix, centers = k, iter.max = 1000)
str(KMC)

# Extract clusters
trainKclusters = KMC$cluster
KMC$centers

train.by.Clust = split(trainMatrix,trainKclusters)

for (i in 1:k){print(sum(train.by.Clust[[i]]))}  #gives count of each cluster


for (i in 1:k){  #will show each clusters largest word's, sorted with largest at the tail
  print (i)
  print(tail(sort(colMeans(train.by.Clust[[i]]))))
}




# Extract clusters

train.norm.Kclusters = KMC$cluster  #associated cluster with observations.  length should equal data nrow
KMC$centers  #shows k-means clusters with varialbes
lapply(split(train, KMC$cluster), colMeans) #shows the cluster averages by non-normed data
mean(train.norm$Happy)


##3.5
library(flexclust)

dim(trainMatrix) = c(nrow(trainMatrix), ncol(trainMatrix))
dim(testMatrix) = c(nrow(testMatrix), ncol(testMatrix))
dim(testSourceMatrix) = c(nrow(testSourceMatrix), ncol(testSourceMatrix))


km.kcca = as.kcca(KMC, trainMatrix)

cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=testMatrix)
table(cluster.train)
table(cluster.test)

################
train1<-subset(train,cluster.train==1)
train2<-subset(train,cluster.train==2)
train3<-subset(train,cluster.train==3)
train4<-subset(train,cluster.train==4)
#train5<-subset(train,cluster.train==5)
#train6<-subset(train,cluster.train==6)

mean(train1$Happy)
mean(train2$Happy)
mean(train3$Happy)
mean(train4$Happy)
