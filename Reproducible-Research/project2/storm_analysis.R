## Coursera Data Science Specialization Reproducible Research
## Project 2 code


## Written by: Michael Gregory
## Date: 22-Oct-2015

## Original data file at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2
##This assignment makes use of data from a NOAA study on storm events from 1950 to 2011.  
##Documentation is available at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf with 
##an FAQ at https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf


##setwd("~/Documents/School/coursera/data science/reproducible research/project2/")

##fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
##tempFile <- tempfile()
##if(capabilities("libcurl")) {
##        download.file(fileURL, tempFile, method = "libcurl")        
##} else {
##        download.file(fileURL, tempFile)
##}

##--------------------------------SWITCH THIS TO LIVE DOWNLOAD------------------------------------------
tempFile <- "StormData.csv.bz2"

#To cut down on processing and cleaning time import only relevant columns of data. 
#Grab the headers list.
headers <- read.csv(tempFile, nrows = 1)
#Ignore all columns except the ones we specify.
headers[,] <- c(rep("NULL", ncol(headers)))
headers[c("FATALITIES", "INJURIES", "PROPDMG", "CROPDMG")] <- "numeric"
headers[c("EVTYPE", "PROPDMGEXP", "CROPDMGEXP")] <- "factor"
#Read in the specified columns
StormDF <- read.table(tempFile, header = TRUE, sep = ",", quote = "\"", fill = TRUE, comment.char = "", colClasses = headers)

##alternatively can set all colClasses manually
##classes <- c("factor", "Date", "character", rep("factor", 8), "Date", "character", rep("factor", 5), rep("numeric", 7), "factor", "numeric", "factor", rep("character", 3), rep("numeric", 4), "character", "numeric")

##or let read.csv guess the colClasses and then update them as needed
##headers <- read.csv(tempFile, nrows = 5)
##classes <- sapply(StormDF, class)
##classes[["STATE__"]] <- "factor"
##classes[["BGN_DATE"]] <- "character"
##...
##StormDF <- read.table(tempFile, header = TRUE, sep = ",", quote = "\"", fill = TRUE, comment.char = "", colClasses = classes)

##--------------------------------SWITCH THIS TO LIVE DOWNLOAD------------------------------------------
##unlink(tempFile)


##preprocess the data
##Check if there NAs
anyNA(StormDF)

##Are the Property Damage and Crop Damage exponent fields accurate?  If not do the incorrect ones represent a statistically relevant set?
allowedExp <- c("k", "K", "M", "m", "B", "b", "0", "0.0", "0.00", NA, "")
nrow(subset(StormDF, !PROPDMGEXP %in% allowedExp | !CROPDMGEXP %in% allowedExp))

##Calculate the full property damage cost by multiplying the exponent
StormDF["PROPDMG.FULL"] <- StormDF["PROPDMG"]
StormDF[StormDF$PROPDMGEXP %in% c("k","K"),"PROPDMG.FULL"] <- StormDF[StormDF$PROPDMGEXP %in% c("k","K"),"PROPDMG"]*1000
StormDF[StormDF$PROPDMGEXP %in% c("m","M"),"PROPDMG.FULL"] <- StormDF[StormDF$PROPDMGEXP %in% c("m","M"),"PROPDMG"]*1000000
StormDF[StormDF$PROPDMGEXP %in% c("b","B"),"PROPDMG.FULL"] <- StormDF[StormDF$PROPDMGEXP %in% c("b","B"),"PROPDMG"]*1000000000
#Do the same for crop damage
StormDF["CROPDMG.FULL"] <- StormDF["CROPDMG"]
StormDF[StormDF$CROPDMGEXP %in% c("k","K"),"CROPDMG.FULL"] <- StormDF[StormDF$CROPDMGEXP %in% c("k","K"),"CROPDMG"]*1000
StormDF[StormDF$CROPDMGEXP %in% c("m","M"),"CROPDMG.FULL"] <- StormDF[StormDF$CROPDMGEXP %in% c("m","M"),"CROPDMG"]*1000000
StormDF[StormDF$CROPDMGEXP %in% c("b","B"),"CROPDMG.FULL"] <- StormDF[StormDF$CROPDMGEXP %in% c("b","B"),"CROPDMG"]*1000000000
#Add Property and Crop Damage
StormDF["TOTALDMG"] <- StormDF$PROPDMG.FULL + StormDF$CROPDMG.FULL



##change zero values in steps to NA
##is.na(activityDF$steps) <- !activityDF$steps

##What is mean total number of steps taken per day?
##1. Calculate the total number of steps taken per day
##sumStepsByDay <- tapply(activityDF$steps, activityDF$date, FUN=sum, na.rm=TRUE)

sumStepsByDay <- aggregate(steps ~ date, FUN=sum, data=activityDF, na.action = "na.omit")

##2. Make a histogram of the total number of steps taken each day (ignoring missing values)
hist(sumStepsByDay$steps, main = "Histogram of Total Steps Taken per Day (ignoring missing values)", xlab = "steps")

##3. Calculate and report the mean and median total number of steps taken per day
library(xtable)
meanSteps <- round(mean(sumStepsByDay$steps))
medianSteps <- round(median(sumStepsByDay$steps))
xt <- xtable(matrix(c(meanSteps, medianSteps), dimnames = list(c("Mean","Median"),c("Steps"))), digits = 0)
print(xt, type="html")

##What is the average daily activity pattern?
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
##meanStepsByInterval <- round(tapply(activityDF$steps, activityDF$interval, FUN=mean, na.rm=TRUE), digits = 2)

meanStepsByInterval <- aggregate(steps ~ interval, FUN=mean, data=activityDF, na.action = "na.omit")
plot(meanStepsByInterval, type="l",
     main = "Average Steps Taken per 5-min Interval", 
     ylab = "Averages steps across all days",
     xlab = "5-min Interval")

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##maxMeanSteps <- meanStepsByInterval[which.max(meanStepsByInterval)]

maxMeanSteps <- meanStepsByInterval[which.max(meanStepsByInterval$steps),]
cat(sprintf("On average, the 5-minute interval %s contains the most (%s) steps.\n", maxMeanSteps$interval, round(maxMeanSteps$steps)))

##Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalNAs <- sum(is.na(activityDF))
cat(sprintf("There are %s rows in the activity dataset containing missing (NA) values.\n", totalNAs))

#2. Create a new column in the dataset with the mean for that 5-minute interval across all days.

activityDF <- merge(activityDF, meanStepsByInterval, by = "interval")
colnames(activityDF) <- c("interval", "steps", "date", "Mean.Daily.Steps")

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityDF[is.na(activityDF$steps),]$steps <- activityDF[is.na(activityDF$steps),]$Mean.Daily.Steps


#4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

imputedSumStepsByDay <- aggregate(steps ~ date, FUN=sum, data=activityDF)
hist(imputedSumStepsByDay$steps, main = "Histogram of Total Steps Taken per Day (with imputed missing values)", xlab = "steps")
imputedMeanSteps <- round(mean(imputedSumStepsByDay$steps))
imputedMedianSteps <- round(median(imputedSumStepsByDay$steps))
imputedXT <- xtable(matrix(c(imputedMeanSteps, imputedMedianSteps), dimnames = list(c("Imputed Mean","Imputed Median"),c("Steps"))), digits = 0)
print(imputedXT, type="html")

if(!meanSteps==imputedMeanSteps) {
        cat(sprintf("As a result of the imputation the average steps per day changed from %s to %s.\n", meanSteps, imputedMeanSteps))
} else {
        cat(sprintf("The average steps per day did not change as a result of the imputation."))
}

if(!medianSteps==imputedMedianSteps) {
        cat(sprintf("As a result of the imputation the median steps per day changed from %s to %s.\n", medianSteps, imputedMedianSteps))
} else {
        cat(sprintf("The median steps per day did not change as a result of the imputation."))
}
#For this dataset imputing using daily average did not have a significant impact on analysis.


##Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

##weekdayNames <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekendNames <- c("Saturday", "Sunday")
activityDF$Type.of.Day <- as.factor(ifelse(weekdays(activityDF$date) %in% weekendNames,"weekend", "weekday"))


#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
        
library(lattice)
meanStepsByDayType <- aggregate(steps ~ interval + Type.of.Day, FUN=mean, data=activityDF, na.action = "na.omit")
xyplot(steps ~ interval | Type.of.Day, meanStepsByDayType, 
       type = "l", 
       layout = c(1,2), 
       main = "Mean Steps Taken per Interval by Day Type", 
       ylab = "Number of Steps (mean)")



