library(randomForest)
library(caret)


attach(ResumeNames)

train <-  sample(1:nrow(ResumeNames),nrow(ResumeNames)/2)
train.all <- ResumeNames[train,]
test.all <- ResumeNames[-train,]

down_train <- downSample(x = train.all[, -ncol(train.all)],
                         y = train.all$call)

down_train <- down_train[,c(0:26)]

up_train <- upSample(x = train.all[,-ncol(train.all)],
                     y = train.all$call)
up_train <- up_train[,c(0:26)]
table(up_train$call)
rf.ResumeNamesDown = randomForest(call~.-name,data = down_train,
                               mtry = 5, importance = TRUE) 
rf.ResumeNamesDown

rf.ResumeNamesUp <- randomForest(call~.-name,data = up_train,
                                 mtry = 5, importance = TRUE)

rf.ResumeNamesUp

yhat.rf <- predict(rf.ResumeNames, newdata = test.all)

mean((yhat.rf == test.all$call)^2)

importance(rf.ResumeNames)
varImpPlot(rf.ResumeNames)
