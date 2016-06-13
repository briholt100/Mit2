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






# Random forest model
library(randomForest)

SimpleModRF = randomForest(Party ~ . -USER_ID -train_set, data=train_df,method='class')

summary(SimpleModRF)
# Make predictions:
predictRF = predict(SimpleModRF, newdata=test_df)

tbl<-table(test_df$Party, predictRF)
tbl

# Accuracy:
(tbl[1]+tbl[4])/sum(tbl)

predictRF = predict(SimpleModRF, newdata=test)

MySubmissionForest<-data.frame(USER_ID = test$USER_ID, Predictions = predictRF)

write.csv(MySubmissionForest, "./kaggle/SubmissionSimpleCForest.csv", row.names=FALSE)

