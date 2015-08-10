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
validationData <- read.csv("/Users/bian/sandbox/practical-ML-R/project/pml-testing.csv")

# split data into training and test sets
inTrain <- createDataPartition(y=myData$classe, p=0.3, list=FALSE)
training <- myData[inTrain,]
testing <- myData[-inTrain,]

##### TRY1 Exploring Data (accuracy 60%)
featurePlot(x=training[,8:11],
            y=training$classe,
            plot = "pairs")
modelFit <- train(classe ~ pitch_belt + roll_belt,
              method="ctree", #ctree, rpart, gbm, rf 
              data=training) 
modelFit  


##### TRY2: Exploring Data (accuracy 70%)
featurePlot(x=training[,10:14],
            y=training$classe,
            plot = "pairs")
modelFit <- train(classe ~ pitch_belt + roll_belt + yaw_belt + total_accel_belt,
                  method="ctree", #ctree, rpart, gbm, rf --> (accuracy=60%)
                  data=training) 
modelFit  

##### TRY3: Exploring Data (accuracy 70%)
featurePlot(x=training[,116:120],
            y=training$classe,
            plot = "pairs")
# training$stddev_yaw_arm_log <- log10(training$stddev_yaw_arm+1) #dealing with the skewed distribution
modelFit <- train(classe ~ pitch_belt + roll_belt + yaw_belt + total_accel_belt + 
                    gyros_belt_y + gyros_belt_z + accel_belt_y +
                    magnet_belt_y + roll_arm + pitch_arm + yaw_arm + avg_roll_arm + accel_arm_x +
                    avg_roll_dumbbell + gyros_dumbbell_x + magnet_dumbbell_x + accel_dumbbell_y + 
                    
                    ,
                  method="ctree", #ctree, rpart, gbm, rf --> (accuracy=60%),
                  preProcess="pca",
                  data=training); modelFit  


###### testset ######
confusionM <- confusionMatrix(predict(modelFit,testing), testing$classe)  
confusionM
confusionM$table
plot(confusionM$table)






