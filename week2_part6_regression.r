## Predicting with Regression

library(caret)
data(faithful)  # eruptions
set.seed(333)
inTrain <- createDataPartition(y=faithful$waiting,  p=0.5, list=FALSE)
trainFaith <- faithful[inTrain,]; testFaith <- faithful[-inTrain,]
head(trainFaith)

# plot
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="duration")


# fit a linear model
lm1 <- lm(eruptions ~ waiting, data=trainFaith)
summary(lm1)
plot(trainFaith$waiting, trainFaith$eruptions, pch=19,col="blue",xlab="waiting",ylab="duration")
lines(trainFaith$waiting, lm1$fitted, lwd=3)

# predict a new value waiting = 80
coef(lm1)[1] + coef(lm1)[2]*80
newdata <-data.frame(waiting=80)
predict(lm1,newdata)

# plot predictions for training and testing
par(mfrow=c(1,2))  # multiple plots
plot(trainFaith$waiting, trainFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="duration")
lines(trainFaith$waiting, lm1$fitted, lwd=3)
plot(testFaith$waiting, testFaith$eruptions, pch=19, col="blue", xlab="waiting", ylab="duration")
lines(testFaith$waiting, predict(lm1,newdata=testFaith), lwd=3)


## get training and test set errors
#RMSE for training
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
#RMSE for testing which is almost always larger than the previous one
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2)) 

# prediction intervals
pred1 <- predict(lm1, newdata=testFaith, interval="prediction")  # I want a prediction interval out
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",col=c(1,2,2), lty=c(1,1,1), lwd=3)


# do it with caret
modelFit <- train(eruptions ~ waiting, data=trainFaith, method="lm")
summary(modelFit$finalModel)


### Predicting with Multivariate Regression
