## Coursera Data Science Specialization - Statistical Inference
## Project code


## Written by: Michael Gregory
## Date: 07-Nov-2015

##In this (part II) of the project I will analyze the ToothGrowth data in the R datasets package. 


##Load, explore and clean the data as necessary.
###-Load the data
library(datasets)
data(ToothGrowth)

###-Explore the data
anyNA(ToothGrowth)
summary(ToothGrowth)
str(ToothGrowth)

pairs(ToothGrowth)
plot(hclust(dist(ToothGrowth)))


perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
perform some basic exploratory data analyses 

##Provide a basic summary of the data.

##Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)

##State your conclusions and the assumptions needed for your conclusions. 



