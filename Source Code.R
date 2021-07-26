library(randomForest)
library(caret)
attach(ResumeNames)


### Random Forests

train <-  sample(1:nrow(ResumeNames),nrow(ResumeNames)/2)
train.all <- ResumeNames[train,]
test.all <- ResumeNames[-train,]

## Down Train

down_train <- downSample(x = train.all,
                         y = train.all$call)

down_train <- down_train[,c(1:27)]

table(up_train$call)
rf.ResumeNamesDown = randomForest(call~.-name,data = down_train,
                                  mtry = 5, importance = TRUE) 
rf.ResumeNamesDown

yhat.rf <- predict(rf.ResumeNamesDown, newdata = test.all)

mean((yhat.rf == test.all$call)^2)

importance(rf.ResumeNamesDown)
varImpPlot(rf.ResumeNamesDown)

## Up Train

up_train <- upSample(x = train.all,
                     y = train.all$call)
up_train <- up_train[,c(1:27)]

rf.ResumeNamesUp <- randomForest(call~.-name,data = up_train,
                                 mtry = 5, importance = TRUE)
rf.ResumeNamesUp

yhat.rf <- predict(rf.ResumeNamesUp, newdata = test.all)

mean((yhat.rf == test.all$call)^2)

importance(rf.ResumeNamesUp)
varImpPlot(rf.ResumeNamesUp)

## Up train with Applicant facing data

rf.ResumeNamesUpApp <- randomForest(call~gender+ethnicity+quality+city+jobs+
                                      experience+honors+volunteer+military+holes+
                                      school+email+computer+special+college,data = up_train,
                                    mtry = 5, importance = TRUE)
rf.ResumeNamesUpApp

yhat.rf <- predict(rf.ResumeNamesUpApp, newdata = test.all)

table(yhat.rf, test.all$call)

mean((yhat.rf == test.all$call)^2)

importance(rf.ResumeNamesUpApp)
varImpPlot(rf.ResumeNamesUpApp)

