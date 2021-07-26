# Import library and dataset
library(AER)
library(randomForest)
library(caret)


attach(ResumeNames)

train <-  sample(1:nrow(ResumeNames),nrow(ResumeNames)/2)
train.all <- ResumeNames[train,]
test.all <- ResumeNames[-train,]

down_train <- downSample(x = train.all,
                         y = train.all$call)

down_train <- down_train[,c(1:27)]

up_train <- upSample(x = train.all[],
                     y = train.all$call)
up_train <- up_train[,c(1:27)]
table(up_train$call)
rf.ResumeNamesDown = randomForest(call~.-name,data = down_train,
                               mtry = 5, importance = TRUE) 
rf.ResumeNamesDown

rf.ResumeNamesUp <- randomForest(call~.-name,data = up_train,
                                 mtry = 5, importance = TRUE)

rf.ResumeNamesUp

yhat.rf <- predict(rf.ResumeNames, newdata = test.all)

mean((yhat.rf == test.all$call)^2)

importance(rf.ResumeNamesDown)
varImpPlot(rf.ResumeNamesDown)

# Data Preprocessing - Linear Regression
gender = as.numeric(ResumeNames$gender)
ethnicity = ifelse(ResumeNames$ethnicity=='cauc', 1, 0)
quality = ifelse(ResumeNames$quality=='low', 0, 1)
call = ifelse(ResumeNames$call=='yes', 1, 0)
city = ifelse(ResumeNames$city=='boston', 1, 0)
honors = ifelse(ResumeNames$honors=='yes', 1, 0)
volunteer = ifelse(ResumeNames$volunteer=='yes', 1, 0)
military = ifelse(ResumeNames$military=='yes', 1, 0)
holes = ifelse(ResumeNames$holes=='yes', 1, 0)
school = ifelse(ResumeNames$school=='yes', 1, 0)
email = ifelse(ResumeNames$email=='yes', 1, 0)
computer = ifelse(ResumeNames$computer=='yes', 1, 0)
special = ifelse(ResumeNames$special=='yes', 1, 0)
college = ifelse(ResumeNames$college=='yes', 1, 0)
equal = ifelse(ResumeNames$equal=='yes', 1, 0)
req = ifelse(ResumeNames$requirements=='yes', 1, 0)
reqexp = ifelse(ResumeNames$reqexp=='yes', 1, 0)
reqcomm = ifelse(ResumeNames$reqcomm=='yes', 1, 0)
reqeduc = ifelse(ResumeNames$reqeduc=='yes', 1, 0)
reqcomp = ifelse(ResumeNames$reqcomp=='yes', 1, 0)
reqorg = ifelse(ResumeNames$reqorg=='yes', 1, 0)

# Scale the variables by normalization
minimum = scale(as.numeric(ResumeNames$minimum)) * sd(call)
experience = scale(ResumeNames$experience)*sd(call)
jobs = scale(ResumeNames$jobs)*sd(call)

resume_applicant = data.frame(jobs, gender, ethnicity, quality, call, honor, volunteer, military, holes, school, computer, special,college)
# Attach all the variables to a new data frame
resume = data.frame(jobs, experience, gender, ethnicity, quality, call, city, honors, volunteer, military, holes, school, email,computer, special,college, equal, req,reqcomm,reqcomp,reqeduc,reqexp,reqorg,minimum)
attach(resume)

# Linear Regression with Backward Selection
lm.fit = lm(call~., data=resume)
summary(lm.fit)
vif(lm.fit)

lm.fit1 = lm(call~.-quality, data=resume)
summary(lm.fit1)
vif(lm.fit1)

lm.fit2 = lm(call~.-(quality+minimum), data=resume)
summary(lm.fit2)
vif(lm.fit2)

lm.fit3 = lm(call~.-(quality+minimum+reqexp), data=resume)
summary(lm.fit3)
vif(lm.fit3)

lm.fit4 = lm(call~.-(quality+minimum+reqexp+military), data=resume)
summary(lm.fit4)
vif(lm.fit4)

lm.fit5 = lm(call~.-(quality+minimum+reqexp+military+reqcomp), data=resume)
summary(lm.fit5)
vif(lm.fit5)

lm.fit6 = lm(call~.-(quality+minimum+reqexp+military+reqcomp+volunteer+gender), data=resume)
summary(lm.fit7)
vif(lm.fit7)


lm.fit7 = lm(call~.-(quality+minimum+reqexp+military+reqcomp+volunteer+gender+computer), data=resume)
summary(lm.fit7)
vif(lm.fit7)


lm.fit8 = lm(call~.-(quality+minimum+reqexp+military+reqcomp+volunteer+gender+college+computer), data=resume)
summary(lm.fit8)
vif(lm.fit8)

# KNN Classification
# Turn Target Variable into factor
resume$call = as.factor(resume$call)

# Split Train and Test set
train <-  sample(1:nrow(resume),nrow(resume)/2)
train.all <- resume[train,]
test.all <- resume[-train,]

down_train <- downSample(x = train.all[, -ncol(train.all)],
                         y = train.all$call)

down_test <- downSample(x = test.all[, -ncol(test.all)],
                         y = test.all$call)

## Downtrain
kk = c(1,3,7,9,10,15,20, 50, 99)
error_vec = vector('numeric',0)
for (i in kk) {
  knn.pred = knn(down_train, down_test, down_train$call, k=i)
  a = table(knn.pred, down_test$call)
  error = (a[1,2]+a[2,1])/sum(a)
  error_vec = c(error_vec, error)
}

plot(x=kk, y=error_vec)


## Uptrain
up_train <- upSample(x = train.all[,-ncol(train.all)],
                     y = train.all$call)

up_test <- upSample(x = test.all[,-ncol(test.all)],
                    y = test.all$call)

kk = c(1,3,7,9,10,15,20, 50, 99)
error_vec = vector('numeric',0)
for (i in kk) {
  
  knn.pred = knn(up_train, up_test, up_train$call, k=i)
  a = table(knn.pred, up_test$call)
  error = (a[1,2]+a[2,1])/sum(a)
  error_vec = c(error_vec, error)
}

plot(x=kk, y=error_vec)




# Only the input of applicant
resume_applicant = data.frame(jobs, gender, ethnicity, quality, call, honors, volunteer, military, holes, school, computer, special,college)
resume_applicant$call = as.factor(resume_applicant$call)

train <-  sample(1:nrow(resume_applicant),nrow(resume_applicant)/2)
train.all <- resume_applicant[train,]
test.all <- resume_applicant[-train,]

##Downtrain
down_train <- downSample(x = train.all[, -ncol(train.all)],
                         y = train.all$call)

down_test <- downSample(x = test.all[, -ncol(test.all)],
                        y = test.all$call)


kk = c(1,3,7,9,10,15,20, 50, 99)
error_vec = vector('numeric',0)
for (i in kk) {
  knn.pred = knn(down_train, down_test, down_train$call, k=i)
  a = table(knn.pred, down_test$call)
  error = (a[1,2]+a[2,1])/sum(a)
  error_vec = c(error_vec, error)
}

plot(x=kk, y=error_vec)



##Uptrain
up_train <- upSample(x = train.all[,-ncol(train.all)],
                     y = train.all$call)

up_test <- upSample(x = test.all[,-ncol(test.all)],
                    y = test.all$call)

kk = c(1,3,7,9,10,15,20, 50, 99)
error_vec = vector('numeric',0)
for (i in kk) {
  
  knn.pred = knn(up_train, up_test, up_train$call, k=i)
  a = table(knn.pred, up_test$call)
  error = (a[1,2]+a[2,1])/sum(a)
  error_vec = c(error_vec, error)
}

plot(x=kk, y=error_vec)

