# WEEK 2
#install.packages("caret")
#install.packages("e1071")

library(caret); library(kernlab); data(spam); 

inTrain <- createDataPartition(y=spam$type,  # the outcome you want to split on
											p=0.75, # 75% to train
											list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)


set.seed(32343)
modelFit <- train(type ~.,  # ~. means use every other variable to predict the type 
						data=training, method="glm")

modelFit  # shows the model accuracy

modelFit$finalModel  # show the final model

predictions <- predict(modelFit, newdata=testing)  # see prediction for testing data 
predictions

confusionMatrix(predictions, testing$type)


####### Data Slicing ####### 

##### K-FOLD
set.seed(32323)
folds <- createFolds(y=spam$type,  # the outcome you want to split on
							k=10,  # number of folds
							list=TRUE,  # return fold is a list  
							returnTrain=TRUE)  # return training sets, if we set if false, then just the test set folds which are much smaller are returned
sapply(folds, length)	
folds[1][1:10]  # seeing the first 10 elements n the first list
						
							
##### RESAMPLING
set.seed(32323)
folds <- createResample(y=spam$type,  # the outcome you want to split on
							times=10,  # number of folds
							list=TRUE)  # return fold is a list  

sapply(folds, length)	
folds[1][1:10]  # seeing the first 10 elements n the first list
							
							
##### TIME SLICES
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme,
						initialWindow=20, # we get a window of 20 values
						horizon=10)  #  we predict the next 20 numbers
names(folds)					
folds$train[[1]]
folds$test[[1]]
			
			
####### Training Options ####### 
library(caret); library(kernlab); data(spam); 

inTrain <- createDataPartition(y=spam$type,  # the outcome you want to split on
											p=0.75, # 75% to train
											list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
modelFit <- train(type ~.,  # ~. means use every other variable to predict the type 
						data=training,
						method="glm")

args(train.default)  # to get default train options
args(trainControl)  # to get default train options

####### Plotting Predictors ####### 
