## CDSS Getting and Cleaning Data
## Course Project

## Written by: Michael Gregory
## Date: 21-Jun-2015

library(dplyr)
library(reshape2)

## Set some general variables
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
trueFileMD5 <- "d29710c9530a31f303801b6bc34bd895"
trueFileVersion <- "1.0"
##setwd("~/Documents/School/coursera/data science/getting and cleaning data/project")
baseDir <- getwd()
zipFile <- paste(baseDir,"/getdata-projectfiles-UCI HAR Dataset.zip", sep="")
zipFileList <- list()
dateDownloaded <- NULL

dataDir <- "UCI HAR Dataset"
outputFile <- paste(baseDir,"/getdata-project-output.txt", sep="")

##If the file doesn't already exist in baseDir download it and check md5
if(!file.exists(zipFile)) {
        cat(sprintf("Downloading file %s \n\t to location %s.\n", fileURL, zipFile))
        download.file(fileURL, destfile=zipFile, method="curl",quiet=TRUE)
        dateDownloaded <- date()
        zipFileMD5 <- digest::digest(algo = "md5", file=zipFile)
        if(zipFileMD5 != trueFileMD5) {
                warning("Downloaded file has changed from original source for this script.  
                        This script may not work as originally intended.")
        }
} else cat(sprintf("Download file already exists in specified location. \n\t Using File: %s \n",zipFile))


## 1. Merges the training and the test sets to create one data set.  

##Since it is not clear we will assume this is the X_train.txt and X_test.txt files.
mergeDF <- NULL
featuresDF <- read.table(unz(zipFile, "UCI HAR Dataset/features.txt"), row.names=NULL)  

for(i in c("test","train")) {
        dataFile <- paste(dataDir,"/",i,"/X_",i,".txt",sep="")
        subjectFile <- paste(dataDir,"/",i,"/subject_",i,".txt",sep="")
        activityFile <- paste(dataDir,"/",i,"/y_",i,".txt",sep="")
        
        cat(sprintf("Reading in \"%s\" dataset files: \n\t%s\n\t%s\n\t%s\n", i,dataFile,subjectFile,activityFile))
        tempDF <- read.table(unz(zipFile, dataFile), row.names=NULL)
        ##assign the column names
        colnames(tempDF) <- featuresDF[,2]
        
        ##prepend subject and activity files as additional columns.
        tempDF <- cbind(read.table(unz(zipFile, activityFile), col.names = "activity", row.names=NULL), tempDF)
        tempDF <- cbind(read.table(unz(zipFile, subjectFile), col.names = "subject", row.names=NULL), tempDF)
        
        if(is.null(mergeDF)) {
                mergeDF <- tempDF
        }
        else {
                cat(sprintf("Merging test and train datasets.\n"))
                mergeDF <- rbind(mergeDF,tempDF)
        }
}


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
cat(sprintf("Extracting only *mean() and *std() variables from dataset.\n"))
mergeDF <- mergeDF[,c(1,2,grep("std\\(\\)|mean\\(\\)",names(mergeDF)))]


## 3. Uses descriptive activity names to name the activities in the data set
cat(sprintf("Adding descriptive activity names.\n"))
activity_labelsDF <- read.table(unz(zipFile, "UCI HAR Dataset/activity_labels.txt"), row.names=NULL)  
mergeDF$activity <- as.character(factor(mergeDF$activity, labels=activity_labelsDF[,2]))


## 4. Appropriately labels the data set with descriptive variable names. 

##need to remove "-" and "()" from column names
cat(sprintf("Cleaning up column names.\n"))
cNames<-colnames(mergeDF)
cNames<-gsub("-","\\.",cNames)
cNames<-gsub("\\(\\)","",cNames)
colnames(mergeDF) <- cNames


## 5. From the data set in step 4, creates a second, independent tidy data set with the average of 
##    each variable for each activity and each subject.
cat(sprintf("Creating summary data frame with subject and activity by mean.\n"))
meltDF<-melt(mergeDF,id.vars=c("subject","activity"))
summaryDF<-dcast(meltDF,subject+activity~...,mean)


##output data set as a txt file created with write.table() using row.name=FALSE
cat(sprintf("Saving summary data frame to \n\t File: %s.\n", outputFile))
write.table(summaryDF, file=outputFile, row.name=FALSE)

