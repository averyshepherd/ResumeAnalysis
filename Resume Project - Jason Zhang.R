rm(list = ls())
library(AER)
data("ResumeNames")

dim(ResumeNames)
?ResumeNames

#KNN Out of Sample Prediction
library(kknn)
tr = sample(1:nrow(ResumeNames), 0.8*nrow(ResumeNames))
admit = ifelse(ResumeNames$call=='no', 0, 1)
ResumeNames = data.frame(ResumeNames, admit)
ResumeNames[,28]
train = ResumeNames[tr,]
test = ResumeNames[-tr,]

out_MSE = NULL

for (i in 2:350) {
  near = kknn(call~.,train,test, k=1, kernel ='rectangular')
  aux = mean((test[,28] - near$fitted)^2)
  
  out_MSE = c(out_MSE, aux)
}

best = which.min(out_MSE)

#Random Forest
library(tree)
tree.ResumeNames = tree(call~.-name,ResumeNames)
summary(tree.ResumeNames)
plot(tree.ResumeNames)
text(tree.ResumeNames, pretty=0)
