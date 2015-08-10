library(AppliedPredictiveModeling)
library(caret)

# Q1.

data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


#Q2. 
data(concrete)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(mixtures$Superplasticizer, main="",xlab="Superplasticizer")  
hist(log10(mixtures$Superplasticizer+1),main="",xlab="Superplasticizer")

#Q3. 
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

names(predictors)
typeColor <- ((training$diagnosis=="Control")*1 + 1)  
preProc <- preProcess(training[,57:68],method="pca",thresh=0.8)
preProc

#Q4.
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
# We need the output and covaraites starting with IL
training = adData[ inTrain,c(1,58:69)] 
testing = adData[-inTrain,c(1,58:69)]


# train the non-PCA model
nonPcaModel <- train(diagnosis~., method="glm", data=training)
nonPcaPrediction <- predict(nonPcaModel, newdata=testing)

# get the accuracy
confusionMatrix(nonPcaPrediction, testing$diagnosis)

# find the PCAs needed for accuracy 80%
preProc <- preProcess(training[,-1],method="pca",thresh=0.8)
trainPca <- predict(preProc,training[,-1])

# traing the model for PCAs
pcaModel <- train(training$diagnosis~., method="glm", data=trainPca)

# get the PCAs of the testing data
testPca <- predict(preProc,testing[,-1])
pcaPrediction <- predict(pcaModel, newdata=testPca)
confusionMatrix(pcaPrediction, testing$diagnosis)




