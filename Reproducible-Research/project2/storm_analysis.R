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
percErrors <- round(nrow(subset(StormDF, !PROPDMGEXP %in% allowedExp | !CROPDMGEXP %in% allowedExp)) / nrow(StormDF), 5)*100
cat(sprintf("Events with erroneous exponent fields represent %s percent of the data.\n", percErrors))


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
StormDF["Economic.Impact"] <- StormDF$PROPDMG.FULL + StormDF$CROPDMG.FULL

#Add Fatalities and Injuries to summarize personal health impact.
StormDF["Health.Impact"] <- StormDF$FATALITIES + StormDF$INJURIES

summary(StormDF$Economic.Impact)
summary(StormDF$Health.Impact)

StormDF[which.max(StormDF$Economic.Impact),]

StormDF[which.max(StormDF$Health.Impact),]



