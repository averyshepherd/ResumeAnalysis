
rm(list = ls())
library(AER)
library(randomForest)
data("ResumeNames")


set.seed(100)
rforest <- randomForest(call~. -name,data=ResumeNames,ntree=100,maxnodes=15)
par(mfrow=c(1,1))
plot(rforest)

varImpPlot(rforest)
