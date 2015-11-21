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

nosim <- 1000
n <- 40
lambda <- 0.2
sampleMatrix <- matrix(rexp(nosim * n, lambda), nosim)
sampleMeans <- apply(sampleMatrix, 1, mean)
sampleVars <- apply(sampleMatrix, 1, var)


##Illustrate via simulation and associated explanatory text the properties of the distribution of the mean of 40 exponentials.  You should
##1. Show the sample mean and compare it to the theoretical mean of the distribution.

##From the exponential distribution we know that the distribution mean is 1/lambda.

distMean <- 1/lambda

##For the sample mean we know that:
##$$
        ##\bar X = \sum_{i=1}^n x_i p(x_i)
##$$
        ##where $p(x_i) = 1/n$

##We could use the mean() function of R but lets calculate it.
calcSampleMean <- sum(sampleMatrix) / (nosim * n)
sampleMean <- mean(sampleMatrix)

##and compare the two results rounded to 10 decimal places.
round((calcSampleMean - sampleMean), 10)

##Now lets compare the sample mean and the distribution mean.
cat(sprintf("The sample mean (%g) is a good approximation of the distrbution mean (%g).\n", round(sampleMean, 4), round(distMean, 4)))


##2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.

##From the exponential distribution we know that the distribution SD (sigma) is 1/lambda and the distribution variance is sigma^2.
distSD <- 1/lambda
distVar <- distSD^2

##We could use R to calculate the sample variance but lets do it manually first.
calcSampleVar <- sum((sampleMatrix - sampleMean)^2) / ((nosim * n) - 1)
sampleVar <- var(c(sampleMatrix))
round((calcSampleVar - sampleVar), 10)

cat(sprintf("The sample variance (%g) is a good approximation of the distrbution variance (%g).\n", round(sampleVar, 4), round(distVar, 4)))


##Likewise we could use R to calculate the sample SD but lets do it manually and compare.
calcSampleSD <- sqrt(calcSampleVar)
sampleSD <- sd(sampleMatrix)
round((calcSampleSD - sampleSD), 10)

cat(sprintf("The sample standard deviation (%g) is a good approximation of the distrbution standard deviation (%g)", round(sampleSD, 4), round(distSD, 4)))

##3. Show that the distribution is approximately normal.

par(mfrow = c(2,2))
hist(sampleMatrix, main = "Histogram of Exponential Variables", xlab = "Exponential Variables")
abline(v=distMean, lwd = 4, col = "red")
legend("topright", "Distribution Mu = 5", lty=1, lwd=2.5, col= "red")

hist(sampleMeans, main = "Histogram of Sample Means", xlab = "Sample Means")
abline(v=distMean, lwd = 4, col = "red")

hist(sampleMatrix, main = "Histogram of Probabilities of Exponential Variables", xlab = "Exponential Variables", prob = TRUE)
curve(dexp(x,lambda), add=TRUE, col = "blue")

hist(sampleMeans, main = "Histogram of Probabilities of Sample Means", xlab = "Sample Means", prob = TRUE)
curve(dnorm(x, mean = sampleMean), add=TRUE, col = "blue")


##g <- ggplot(data = data.frame(c(sampleMeans)), aes(x = c.sampleMeans.)) 
##g <- g + geom_histogram(aes(y = ..density..), fill = "lightblue", binwidth=1, colour = "black")
##g <- g + geom_density(size = 2, colour = "black")
##g


