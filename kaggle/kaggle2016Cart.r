#imputed_df<-read.csv("./kaggle/imputed_df.csv")
#return df back to test and train, split train set


train<-df[which(df$train_set==T),]
test<-df[which(df$train_set==F),]
nrow(test)
nrow(train)

set.seed(144)
spl = sample(1:nrow(train), size=0.7*nrow(train))
train_df = train[spl,]
test_df = train[-spl,]






#cart

library(rpart)
library(rpart.plot)

simpleCartMod1<-rpart(Party ~ Q124742 + Q119851 + Q118232 + Q118233 + 
                        Q116881 +  Q116953 + Q116601 + Q115611+
                        Q109244 + Q106272 + Q104996 + Q101163 +
                        Q98197 + Income +
                        HouseholdStatus + EducationLevel + age, data=train_df, method="class")
prp(simpleCartMod1)

predCartTrain1<-predict(simpleCartMod1,newdata=test_df,type='class')
tbl<-table(test_df$Party,predCartTrain1)
tbl
(tbl[1]+tbl[4])/sum(tbl)


predCart1<-predict(simpleCartMod1,newdata=test,type='class')

MySubmissionCart = data.frame(USER_ID = test$USER_ID, Predictions = predCart1)

write.csv(MySubmissionCart, "./kaggle/SubmissionSimpleCart.csv", row.names=FALSE)
