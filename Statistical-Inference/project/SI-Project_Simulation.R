## Coursera Data Science Specialization - Statistical Inference
## Project code


## Written by: Michael Gregory
## Date: 07-Nov-2015

##In this (part I) of the project I will investigate the exponential distribution in R and compare it with the Central Limit Theorem. The 
##exponential distribution can be simulated in R with rexp(n, lambda) where lambda is the rate parameter. The mean of exponential distribution 
##is 1/lambda and the standard deviation is also 1/lambda. Set lambda = 0.2 for all of the simulations. I will investigate the distribution 
##of averages of 40 exponentials. I will do a thousand simulations.

##setwd("~/Documents/School/coursera/data science/statistical inference//project")

set.seed(11072015)
setLambda <- .2
simCount <- 10000
varCount <- 40

Exponential.Variables <- NULL
Exponential.Means <- NULL
for (i in 1 : simCount) {
        Exponential.Variables[[i]] <- rexp(varCount, setLambda)
        Exponential.Means <- c(Exponential.Means, mean(Exponential.Variables[[i]]))
}

expDF <- t(as.data.frame(Exponential.Variables))

##Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials.  You should
##1. Show the sample mean and compare it to the theoretical mean of the distribution.

sampleMean <- mean(unlist(Exponential.Variables))
distMean <- 1/setLambda

##2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

distSD <- 1/setLambda
distVar <- distSD^2

sampleSD <- sd(unlist(Exponential.Variables))
sampleVar <- var(unlist(Exponential.Variables))


##3. Show that the distribution is approximately normal.

par(mfrow = c(2,2))
hist(unlist(Exponential.Variables), main = "Histogram of Exponential Variables", xlab = "Exponential Variables")
hist(Exponential.Means, main = "Histogram of Sample Means", xlab = "Sample Means")

hist(unlist(Exponential.Variables), main = "Histogram of Probabilities of Exponential Variables", xlab = "Exponential Variables", prob = TRUE)
curve(dexp(x,setLambda), add=TRUE, col = "blue")

hist(Exponential.Means, main = "Histogram of Probabilities of Sample Means", xlab = "Sample Means", prob = TRUE)
curve(dnorm(x, mean = sampleMean), add=TRUE, col = "blue")

