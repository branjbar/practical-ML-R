
#install.pakcages("ISLR"); 

library(ISLR); # Data for An Introduction to Statistical Learning with Applications in R
library(ggplot2); library(caret);

data(Wage)
summary(Wage)

# first we set aside the testing set and don't look at it!
inTrain <- createDataPartition(y=Wage$wage,
								p = 0.7, list=FALSE)
								
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training) ; dim(testing)


# plotting all the features and the outcome in an 4x4 grid
featurePlot(x=training[,c("age", "education","jobclass")],
			y = training$wage,
			plot="pairs")

# using qplot to look at specific graphs
qplot(age, wage, data=training)


# in the previous plot we saw some patterns, so we color it for a better understanding
qplot(age, wage, colour=jobclass, data=training)


# we can also add regression smoothers
qq <- qplot(age, wage, color=education, data=training)  # colored by education
qq + geom_smooth(method='lm', formula=y~x)  # apply linear smoother to data



# we want to categorize the data based on wage quantiles
#> install.packages("Hmisc")
library(Hmisc)
cutWage <= cut2(training$wage, g=3)
table(cutWage)

# using boxplots for each quantiles group
p1 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot"))
grid.arrange(p1,p2,ncol=2)


# it would be cool to add the data points on top of the boxplot too
p2 <- qplot(cutWage, age, data=training, fill=cutWage, geom=c("boxplot", "jitter"))
library(gridExtra)
grid.arrange(p1,p2,ncol=2)

# use tables to use factorized version of the cut variable to look at the other data points
t1 <- table(cutWage,training$jobclass)
t1
prop.table(t1,1)  # get the proportions

# finally we use the density plots
qplot(wage, color=education, data=training, geom="density")
			
