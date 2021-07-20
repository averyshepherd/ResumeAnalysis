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


ResumeNames.ds = upSample(x = ResumeNames[, -ncol(ResumeNames)],
                                    y = ResumeNames$call)


ResumeNames.train = sample(1:nrow(ResumeNames.ds), nrow(ResumeNames.ds)/2)
ResumeNames.test=ResumeNames.ds[-ResumeNames.train ,"call"]

bag.ResumeNames= randomForest(call~.-name,data=ResumeNames.ds, subset=ResumeNames.train,
                         mtry=4,importance =TRUE)
bag.ResumeNames


yhat.bag = predict(bag.ResumeNames , newdata=ResumeNames.ds[-ResumeNames.train ,])
plot(yhat.bag , ResumeNames.test)
abline (0,1)
mean((yhat.bag - as.numeric(ResumeNames.test))^2)




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

library(gbm)
set.seed(1)
boost.resume=gbm(call~.-name,data=up_train, distribution="gaussian",
                 n.trees=5000, interaction.depth=4)
summary(boost.resume)

par(mfrow=c(1,2))
plot(boost.resume, i="wanted")
plot(boost.resume, i="requirements")

# predict call on test set
yhat.boost=predict(boost.resume, newdata =ResumeNames[-train,],
                   n.trees=5000)
mean((yhat.boost - as.numeric(ResumeNames[-train,"call"]))^2)

# using lambda = .2
boost.resume=gbm(call~.-name,data=up_train, distribution="gaussian",
                 n.trees=5000, interaction.depth=4, shrinkage = .2)
summary(boost.resume)

# predict call on test set
yhat.boost=predict(boost.resume, newdata =ResumeNames[-train,],
                   n.trees=5000)
mean((yhat.boost - as.numeric(ResumeNames[-train,"call"]))^2)





