library(AER)
data("ResumeNames")
library(caret)
library(randomForest)

train <-  sample(1:nrow(ResumeNames),nrow(ResumeNames)/2)
train.all <- ResumeNames[train,]
test.all <- ResumeNames[-train,]

up_train <- upSample(x = train.all,
                     y = train.all$call)
up_train <- up_train[,c(0:26)]

ResumeNames.up = upSample(x = ResumeNames[, -ncol(ResumeNames)],
                          y = ResumeNames$call)

rf.ResumeNamesUp <- randomForest(call~gender+ethnicity+quality+city+jobs+
                                   experience+honors+volunteer+military+holes+
                                   school+email+computer+special+college,data = up_train,
                                 mtry = 5, importance = TRUE)

rf.ResumeNamesUp

yhat.rf <- predict(rf.ResumeNamesUp, newdata = test.all)

table(yhat.rf, test.all$call)


mean((yhat.rf == test.all$call)^2)

importance(rf.ResumeNamesUp)
varImpPlot(rf.ResumeNamesUp)
