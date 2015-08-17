## DATA Source: http://groupware.les.inf.puc-rio.br/har
# Six young health participants were asked to perform one set of 10 repetitions 
# of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly 
# according to the specification (Class A), throwing the elbows to the front (Class B), 
# lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) 
# and throwing the hips to the front (Class E).

# here are sample solutions of the others:
# https://rpubs.com/jocaqui/26619
# https://github.com/jeffheaton/jhi-practical-machine-learning/blob/master/PracticalMachineLearning.RMD
# http://emres.github.io/practical-ml/
# http://rstudio-pubs-static.s3.amazonaws.com/20131_7fca728506724c27bc629081983f0ec2.html


# initialization
set.seed(3421)
library(caret)

# load data
setwd("/Users/bian/sandbox/practical-ML-R/project/")
myData <- read.csv("pml-training.csv", header=T,  na.strings=c("NA", "#DIV/0!"))
validationData <- read.csv("pml-testing.csv")

# split data into training and test sets
inTrain <- createDataPartition(y=myData$classe, p=1, list=FALSE)
training <- myData[inTrain,]
testing <- myData[-inTrain,]

###### Explore Data and choose useful features ######
useful_features = c("pitch_belt","roll_belt","yaw_belt","total_accel_belt","gyros_belt_y",
                    "gyros_belt_z","accel_belt_y","magnet_belt_y","roll_arm","pitch_arm",
                    "yaw_arm","avg_roll_arm","accel_arm_x","avg_roll_dumbbell","gyros_dumbbell_x",
                    "magnet_dumbbell_x","accel_dumbbell_y","roll_forearm", "magnet_dumbbell_z",
                    "max_roll_forearm","min_pitch_forearm","classe")
  
training_useful <- training[,useful_features]
testing_useful <- testing[,useful_features]

###### IMPUTE NA Values ######
preObj <- preProcess(training_useful[,-22], method="knnImpute") 
knnTrain <- predict(preObj, training_useful[,-22])
knnTrain$classe <- training_useful$classe

###### TRAIN ######
tc = trainControl(method = "cv", number = 4)
modelFit <- train(classe~.,
                  method="rf", #ctree, rpart, gbm, rf --> (accuracy=60%),
                  trControl = tc,
                  prox = TRUE,
                  allowParallel = TRUE,
                  preProcess=c("pca"),
                  data=knnTrain); modelFit  


###### TEST ######
knnTest <- predict(preObj, testing_useful[,-22])
confusionM <- confusionMatrix(predict(modelFit,knnTest), testing_useful$classe)  
confusionM
confusionM$table
plot(confusionM$table)


## Validation
validating <- validationData[useful_features[-22]]
knnValid <- predict(preObj, validating[,-22])
answers = predict(modelFit,knnValid)

## Submit Answers
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)






