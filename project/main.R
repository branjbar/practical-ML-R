## DATA Source: http://groupware.les.inf.puc-rio.br/har
# Six young health participants were asked to perform one set of 10 repetitions 
# of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly 
# according to the specification (Class A), throwing the elbows to the front (Class B), 
# lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) 
# and throwing the hips to the front (Class E).

# initialization
set.seed(3421)
library(caret)

# load data
myData <- read.csv("/Users/bian/sandbox/practical-ML-R/project/pml-training.csv")

# split data into training and test sets
inTrain <- createDataPartition(y=myData$classe, p=0.8, list=FALSE)
training <- myData[inTrain,]
testing <- myData[-inTrain,]


# I start with a predictive model with three covariates
modelFit <- train(classe ~ roll_arm + pitch_arm + yaw_arm,
              method="rf", #ctree, rpart, gbm 
              data=training)

modelFit  # accuracy is 0.22

confusionM <- confusionMatrix(predict(modelFit,testing), testing$classe)  
confusionM$table
plot(confusionM$table)





