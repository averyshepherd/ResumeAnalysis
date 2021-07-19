library(AER)
data("ResumeNames")
set.seed(1)
library(caret)
library(randomForest)


# train.idx = sample(1:nrow(ResumeNames), nrow(ResumeNames)/2)
# train.idx = createDataPartition(ResumeNames$call, p = .5, list = F, times = 1)
# ResumeNames.train = ResumeNames[train.idx,]
# ResumeNames.test = ResumeNames[-train.idx,]
# 
# 
# # down sample
# ResumeNames.traindown = downSample(x = ResumeNames.train[, -ncol(ResumeNames.train)], 
#                                    y = ResumeNames.train$call)
# ResumeNames.testdown = downSample(x = ResumeNames.test[, -ncol(ResumeNames.test)], 
#                                   y = ResumeNames.test$call)
# 
# 
# bag.ResumeNames= randomForest(call~. -name,data=ResumeNames.traindown,
#                               mtry=25,importance =TRUE)
# bag.ResumeNames
# 
# yhat.bag = predict(bag.ResumeNames , newdata=ResumeNames.testdown)
# 
# plot(yhat.bag, ResumeNames.traindown[,"call"])
# abline (0,1)
# mean((yhat.bag -ResumeNames.traindown[,"call"])^2)
# 
# # change number of trees
# bag.ResumeNames= randomForest(medv~.,data=ResumeNames , subset=train ,
#                          mtry=13,ntree=25)
# yhat.bag = predict(bag.ResumeNames , newdata=ResumeNames[-train ,])
# mean((yhat.bag -ResumeNames.train)^2)


ResumeNames.ds = downSample(x = ResumeNames[, -ncol(ResumeNames)],
                                    y = ResumeNames$call)


ResumeNames.train = sample(1:nrow(ResumeNames.ds), nrow(ResumeNames.ds)/2)
ResumeNames.test=ResumeNames.ds[-ResumeNames.train ,"call"]

bag.ResumeNames= randomForest(call~.-name,data=ResumeNames.ds, subset=ResumeNames.train,
                         mtry=25,importance =TRUE)
bag.ResumeNames

yhat.bag = predict(bag.ResumeNames , newdata=ResumeNames.ds[-ResumeNames.train ,])
plot(yhat.bag , ResumeNames.test)
abline (0,1)
mean((yhat.bag - as.numeric(ResumeNames.test))^2)


