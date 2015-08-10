## 
library(caret)
# load data
myData <- read.csv("/Users/bian/sandbox/practical-ML-R/project/pml-training.csv")

# split data into training and test sets
inTrain <- createDataPartition(y=myData$classe, p=0.6, list=FALSE)
training <- myData[inTrain,]
testing <- myData[-inTrain,]



