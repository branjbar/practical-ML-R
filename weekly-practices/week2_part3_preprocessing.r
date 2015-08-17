library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y=spam$type, 
								p=0.75,
								list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve, main="",xlab="ave. capital run length")  # this variable is very skewed

# to see how skewed data is!!
mean(triaing$captalAve)
sd(training@capitalAVe)


# we can do standardization
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve)) / sd(trainCapAve)

mean(trainCapAveS)
sd(trainCapAveS)

# when applying this to the testset, use mean and variance of trainingset
testCapAve <- testing$capitalAve
testCapAveS <- (testCapAve - mean(trainCapAve)) / sd(trainCapAve)

mean(testCapAveS)
sd(testCapAveS)


## use preProcess function to do the standardizaitons
preObj <- preProcess(training[,-58],  # get every variable exept the 58th one which is the output
									method=c("center","scale"))  # center everyone and scale them
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS)
sd(trainCapAveS)

# and we can use the preprocessing object to predict the values of testing set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS)


## we can pass the preProcess argument to caret predictor

set.seed(32343)
modelFit <- train(type ~., 
						data=training,
						preProcess=c("center", "scale"),
						method="glm")
modelFit


## Using Box-Cos transformation which 
preObj <- preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

# dealing with missing data
set.see(13343)

# Make some NA values
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1], size=1,prob=0.5)==1   # generates random values
training$capAve[selectNA] <- NA


# Impute and standardize
library(RANN)  # Fast Nearest Neighbour Search
preObj <- preProcess(training[,-58], method="knnImpute") 
capAve <- predict(preObj, training[,-58])$capAve

# Standardize true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth)) / sd(capAveTruth)


							
# we can compare the imputed values with the true values which were there

quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])


