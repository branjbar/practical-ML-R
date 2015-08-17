
## install the library if needed
# install.packages("kernlab")

## load the library file and set a seed number   
library(kernlab); data(spam); set.seed(987)

## get 50 samples
smallSpam <- spam[sample(dim(spam)[1], size=50),]

## assign 1 or 2 to the type of sample data
spamLabel <- (smallSpam$type=="spam")*1 + 1

## plot
#plot(smallSpam$capitalAve, col=spamLabel)

## based on the visual differences, design strict predictor function
rule1 <- function(x) {
	prediction <- rep(NA, length(x))
	prediction[x>2.7] <- "spam"
	prediction[x<2.4] <- "nonspam"
	prediction[(x>= 2.4 & x<=2.45)] <- "spam"
	prediction[(x>2.45 & x<= 2.70)] <- "nonspam"
	return(prediction)
}

## here we have a simpler but more robust rule
rule2 <- function(x) {  
	prediction <- rep(NA, length(x))
	prediction[x>2.8] <- "spam"
	prediction[x<=2.8] <- "nonspam"
	return(prediction)
}


## show the number of false/true positive/negatives in table for sample data and the complete data
table(rule1(smallSpam$capitalAve), smallSpam$type)
table(rule1(spam$capitalAve), spam$type)

sum(rule1(spam$capitalAve)==spam$type)
sum(rule2(spam$capitalAve)==spam$type)
