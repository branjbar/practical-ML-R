## Predicting with Regression Multiple Covariates

library(ISLR); library(ggplot2); library(caret);
data(Wage); Wage <- subset(Wage, select =- c(logwage))  # remove column logwage which we will predict
summary(Wage)

# getting training/test sets
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)
training <- Wage[inTrain,]; testing <- Wage[-inTrain,]
dim(training); dim(testing)


# Feature Plot
featurePlot(x=training[,c("age", "education", "jobclass")],
			y=training$wage,
			plot="pairs")
			
# plot age versus wage
qplot(age, wage, data=training)

# plot age versus wage color by jobclass
qplot(age, wage, color=jobclass, data=training)

# plot age versus wage color by education
qplot(age, wage, color= education, data=training)

# fit a linear model
modelFit <- train(wage ~ age + jobclass + education, method="lm", data=training)
modelFit

finModel <- modelFit$finalModel
finModel

# Diagnostics
plot(finModel,  # plot the predicted values vs the residuals, outliers will be plotted too.
	1, # this 1 uses the plot1. other numbers fill give you other plots
	pch=18,cex=0.5,col="#00000010")  

# color by variables which are not used in the model
qplot(finModel$fitted, finModel $residuals, color=race, data=training)

# plot by index
plot(finModel$residuals,pch=19)  # if you see a trend, it means there is a vriable that you're missing. something that rows are order by


# predicted versus truth in test set
pred <- predict(modelFit, testing)
qplot(wage,pred,color=year,data=testing)


# if you want all the covariates in your model
modelFitAll <- train(wage ~ .,data=training,method="lm")
pred <- predict(modelFitAll, testing)
qplot(wage,pred,data=testing)

