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




#step
SimpleModStep <- step(SimpleMod)
summary(SimpleModStep)

PredTrain = predict(SimpleModStep,newdata=train_df, type="response")
PredTest = predict(SimpleModStep, newdata=test, type="response")

threshold = 0.5
PredTrainLabels = as.factor(ifelse(PredTrain<threshold, "Democrat", "Republican"))
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

tbl<-table(train_df$Party,PredTrain>.50)
tbl
(tbl[1]+tbl[4])/sum(tbl)
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "./kaggle/SubmissionSimpleLogStep.csv", row.names=FALSE)

