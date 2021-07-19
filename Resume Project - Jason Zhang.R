rm(list = ls())
library(AER)
data("ResumeNames")

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
minimum = as.numeric(ResumeNames$minimum) 
resume = data.frame(ResumeNames$jobs, ResumeNames$experience, gender, ethnicity, quality, call, city, honors, volunteer, military, holes, school, email,computer, special,college, equal, req,reqcomm,reqcomp,reqeduc,reqexp,reqorg,minimum)
attach(resume)



# K-fold validation - gender & ethnicity
train = data.frame(call,gender,ethnicity)
test = data.frame(call,gender, ethnicity)

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






#Random Forest
library(tree)
tree.ResumeNames = tree(call~.-name,ResumeNames)
summary(tree.ResumeNames)
plot(tree.ResumeNames)
text(tree.ResumeNames, pretty=0)
