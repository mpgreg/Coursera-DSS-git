## CDSS Getting and Cleaning Data
## Course Project

## Written by: Michael Gregory
## Date: 13-Jun-2015

library(plyr)

## Set some general variables
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
trueFileMD5 <- "d29710c9530a31f303801b6bc34bd895"
trueFileVersion <- "1.0"
baseDir <- "~/Documents/School/coursera/data science/getting and cleaning data/project"
zipFile <- paste(baseDir,"/getdata-projectfiles-UCI HAR Dataset.zip", sep="")
zipFileList <- list()
dateDownloaded <- NULL

dataDir <- "UCI HAR Dataset"
fullDataDir <- paste(baseDir, "/", dataDir, sep="")
testDir <- paste(dataDir, "/test", sep="")
trainDir <- paste(dataDir, "/train", sep="")

##If the file doesn't already exist in baseDir download it and check md5
if(!file.exists(zipFile)) {
        download.file(fileURL, destfile=zipFile, method="curl")
        dateDownloaded <- date()
        zipFileMD5 <- digest::digest(algo = "md5", file=zipFile)
        if(zipFileMD5 != trueFileMD5) {
                warning("Downloaded file has changed from original source for this script.  
                        This script may not work as originally intended.")
        }
}

## 1. Merges the training and the test sets to create one data set.  

##Since it is not clear we will assume this is the X_train.txt and X_test.txt files.

xTrainFile <- 
xTestFile <- 

##extract all file and directory names from the zip file
zipFileList <- unzip(zipFile, list=TRUE)

##for each file in "test" directory create an data frame
print("Joining all files for test dataset")

##extract only the filenames (not directories) from the "test" subdirectory
searchString <- paste(testDir,".*txt",sep="")
testFileList <- zipFileList[grep(searchString,zipFileList$Name),]$Name

testDF <- NULL  ##set to NULL to create initial cbind

for( readFile in testFileList) {
        ##get the filename to use as col.name
        baseColName <- strsplit(tail(strsplit(readFile,"/")[[1]],1),"_test.txt")
        colNamePrefix <- paste(baseColName,"_",sep="")
        ##read into a temporary DF
        tempDF <- read.table(unz(zipFile,  readFile ))
        print(dim(tempDF))
        ##assign the column names
        colnames(tempDF) <- NULL
        colnames(tempDF) <- colnames(tempDF, do.NULL = FALSE, prefix = colNamePrefix)
        print(readFile)
        
        if(is.null(testDF)) {
                testDF <- tempDF
        }
        else {
                testDF <- cbind(testDF,tempDF)
        }
}

##for each file in "train" directory create an data frame
print("Joining all files for train dataset")

##extract only the filenames (not directories) from the "train" subdirectory
searchString <- paste(trainDir,".*txt",sep="")
trainFileList <- zipFileList[grep(searchString,zipFileList$Name),]$Name

trainDF <- NULL  ##set to NULL to create initial cbind

for( readFile in trainFileList) {
        ##get the filename to use as col.name
        baseColName <- strsplit(tail(strsplit(readFile,"/")[[1]],1),"_train.txt")
        colNamePrefix <- paste(baseColName,"_",sep="")
        ##read into a temporary DF
        tempDF <- read.table(unz(zipFile,  readFile ))
        print(dim(tempDF))
        ##assign the column names
        colnames(tempDF) <- NULL
        colnames(tempDF) <- colnames(tempDF, do.NULL = FALSE, prefix = colNamePrefix)
        print(readFile)
        
        if(is.null(trainDF)) {
                trainDF <- tempDF
        }
        else {
                trainDF <- cbind(trainDF,tempDF)
        }
}


##print("Appending test and train data frames")
mergeDF <- rbind(testDF,trainDF)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 




## 3. Uses descriptive activity names to name the activities in the data set

##activity_labelsDF <- read.table(unz(zipFile, "UCI HAR Dataset/activity_labels.txt"), row.names=1)  


## 4. Appropriately labels the data set with descriptive variable names. 

##featuresDF <- read.table(unz(zipFile, "UCI HAR Dataset/features.txt"), row.names=1)  

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of 
##    each variable for each activity and each subject.



