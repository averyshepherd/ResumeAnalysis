# Read in the dataset
rm(list = ls())
library(AER)
data("ResumeNames")

# Take a look at the data set
dim(ResumeNames)
summary(ResumeNames)

#KNN Out of Sample Prediction
library(kknn)

# Turn all the factor variables into numeric variables
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

# Attach all the variables to a new data frame
resume = data.frame(jobs, experience, gender, ethnicity, quality, call, city, honors, volunteer, military, holes, school, email,computer, special,college, equal, req,reqcomm,reqcomp,reqeduc,reqexp,reqorg,minimum)
attach(resume)


# Since the dataset is very biased on the no side, so we pick out the yes and randomly choose the same length of no to create a new dataset.
set.seed(142)
no = subset(resume, call == 0)
yes_set = subset(resume, call ==1)
no_sample = sample(1:dim(no)[1], dim(yes_set)[1])
no_set = no[no_sample,]
whole = rbind(no_set, yes_set)
row = sample(nrow(whole))
rand_whole = whole[row,]
attach(rand_whole)


# K-fold validation - gender & ethnicity
train = data.frame(call,gender,ethnicity, school, college, equal, experience, job)
test = data.frame(call,gender,ethnicity, school, college, equal, experience, job)

kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(call~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
  
}


mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)

# K-fold validation - gender, ethnicity, college
train = data.frame(call,gender,ethnicity, college)
test = data.frame(call,gender, ethnicity, college)

kcv = 10
n0 = round(n/kcv,0)

out_MSE = matrix(0,kcv,100)

used = NULL
set = 1:n

for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for(i in 1:100){
    
    near = kknn(call~.,train_i,test_i,k=i,kernel = "rectangular")
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  
  used = union(used,val)
  set = (1:n)[-used]
  
  cat(j,'\n')
  
}


mMSE = apply(out_MSE,2,mean)

plot(log(1/(1:100)),sqrt(mMSE),xlab="Complexity (log(1/k))",ylab="out-of-sample RMSE",col=4,lwd=2,type="l",cex.lab=1.2,main=paste("kfold(",kcv,")"))
best = which.min(mMSE)
text(log(1/best),sqrt(mMSE[best])+0.1,paste("k=",best),col=2,cex=1.2)
text(log(1/2),sqrt(mMSE[2])+0.3,paste("k=",2),col=2,cex=1.2)
text(log(1/100)+0.4,sqrt(mMSE[100]),paste("k=",100),col=2,cex=1.2)

best



#Tree Method
library(tree)
tree.resume = tree(call~.,rand_whole)
summary(tree.resume)
plot(tree.resume)
text(tree.resume, pretty=0)

#Regression Tree
train = sample(1:nrow(rand_whole), nrow(rand_whole)/2)
tree.resume = tree(call~., data=rand_whole, subset =train)
summary(tree.resume)
cv.resume = cv.tree(tree.resume)
plot(cv.resume$size, cv.resume$dev, type='b')

prune.resume = prune.tree(tree.resume, best =2)
plot(prune.resume)
text(prune.resume, pretty = 0)

#Random Forest
library(randomForest)
rf.resume = randomForest(call~., data=rand_whole, subset=train, mtry = 3, importance=TRUE)
