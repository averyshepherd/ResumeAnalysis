library(AER)
data("ResumeNames")
library(caret)
library(randomForest)


ResumeNames.up = upSample(x = ResumeNames[, -ncol(ResumeNames)],
                          y = ResumeNames$call)


ResumeNames.train = sample(1:nrow(ResumeNames.up), nrow(ResumeNames.us)/2)
ResumeNames.test=ResumeNames.ds[-ResumeNames.train ,"call"]