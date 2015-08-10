

library(ISLR); library(caret); data(Wage)
inTrain <- createDataPartition(y=Wage$wage,
								p=0.7,
								list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]


# convert factor variables to indicator variables
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training)# the outcome is wage and jobclass  is the predicted variable.  

head(predict(dummies,newdata=training))


# removing zero covariates
nsv <- nearZeroVar(training,saveMetrics=TRUE)
nsv

## spline basis
library(splines)
bsBasis <- bs(training$age, df=3)  # fit a polynomial to age
lm1 <- lm(wage ~ bsBasis, data=training)  # 
plot(training$age, training$wage, pch=19, cex=0.5)  # pch for marker and cex for scale
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=0.5) plotting the fit spline

# splines on the test set
predict(bsBasis, age=testing$age)  # please note that we're using the same bsBasis which is fit to the training data
