#imputed_df<-read.csv("./kaggle/imputed_df.csv")
#return df back to test and train, split train set

set.seed(144)
spl = sample(1:nrow(train), size=0.7*nrow(train))
train_df = train[spl,]
test_df = train[-spl,]


# Simple logistic regression model

SimpleMod = glm(Party ~ . -USER_ID -train_set, data=train_df, family=binomial)
summary(SimpleMod)

#The following variables were significant; note that 98869 and 98197 are high cor.
sig_var<-c('Q124742',' Q119851',' Q118232',' Q118233',
           ' Q116881',' Q116953','Q116601','Q115611',
           'Q109244','Q106272','Q104996','Q101163',
           'Q98869','Q98197','Income',
           'HouseholdStatus','EducationLevel','age')
SimpleMod1 = glm(Party ~ Q124742 + Q119851 + Q118232 + Q118233 + 
                  Q116881 +  Q116953 + Q116601 + Q115611+
                  Q109244 + Q106272 + Q104996 + Q101163 +
                  Q98197 + Income +
                  HouseholdStatus + EducationLevel + age, data=train_df, family=binomial)
summary(SimpleMod1)




# And then make predictions on the test set:
PredTrain = predict(SimpleMod1,newdata=train_df, type="response")
PredTest = predict(SimpleMod1, newdata=test, type="response")

threshold = 0.49
PredTrainLabels = as.factor(ifelse(PredTrain<threshold, "Democrat", "Republican"))
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

tbl<-table(train_df$Party,PredTrain>.49)
tbl
(tbl[1]+tbl[4])/sum(tbl)
# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.

# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)

write.csv(MySubmission, "./kaggle/SubmissionSimpleLog.csv", row.names=FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition

# This model was just designed to help you get started - to do well in the competition, you will need to build better models!