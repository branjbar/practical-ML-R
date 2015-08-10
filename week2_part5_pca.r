## Principap Components Analysis (PCA)
library(caret); library(kernlab); data(spam)

inTrain <- createDataPartition(y=spam$type,p=0.75,list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

M <- abs(cor(training[,-58]))  # Find the correlation and take their absolute value and leave out the 58 column which is outcome
diag(M) <- 0  # the correlation of everything with itself is zero
which(M > 0.8,arr.ind=T)


## PCA for a very small subset of span feature space
smallSpam <- spam[,c(34,32)]
prComp <- prcomp(smallSpam)  # principle component analysis
plot(prComp$x[,1], prComp$x[,2])  # the first principle component and the second one

prComp$rotation  # to see the rotation

## PCA for spam data complete
typeColor <- ((spam$type=="spam")*1 + 1)  # color is black if it's not spam
prComp <-prcomp(log10(spam[,-58]+1))  # just to make data to look more normal
p1 <- qplot(prComp$x[,1],prComp$x[,2],col=typeColor,xlab="PC1",ylab="PC2")
p2 <- qplot(prComp$x[,2],prComp$x[,3],col=typeColor,xlab="PC1",ylab="PC2")

grid.arrange(p1,p2,ncol=2)

## we can do this by caret as well
typeColor <- ((spam$type=="spam")*1 + 1)  # color is black if it's not spam
preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
spamPC <- predict(preProc,log10(spam[,-58]+1))
plot(spamPC[,1],spamPC[,2],col=typeColor)


## PCA and training
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
trainPC <- predict(preProc,log10(training[,-58]+1))
modelFit <- train(training$type ~. ,method="glm",data=trainPC)  # here we just pass the principle components

# testing
testPC <- predict(preProc, log10(testing[,-58]+1))
confusionMatrix(testing$type, predict(modelFit,testPC))

# we can also pass the pca into the training model
modelFit <- train(training$type ~., method="glm", preProcess="pca", data=training)
confusionMatrix(testing$type,predict(modelFit,testing))

