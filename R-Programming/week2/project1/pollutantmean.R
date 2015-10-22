pollutantmean <- function(directory, pollutant, id = 1:332) {
        ##Written by: Michael Gregory
        ##email: michaelgregory122@gmail.com
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        ##sample values for testing
        ##id <- c(1,5,200)
        ##directory <- "/Volumes/Data/Users/mpgregor/Documents/School/coursera/data science/R Programming/week 2/prog assignment 1/specdata"
        ##pollutant <- "sulfate"
        
        
        ##Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' 
        ##particulate matter data from the directory specified in the 'directory' argument 
        
        ## error checking
        ##check to make sure pollutant is a valid value
        if(pollutant != "sulfate" && pollutant != "nitrate") {
                print("Invalid pollutant")
                return
        }
        
        ##check to make sure id is a non-NA vector of lenght at least 1
        if (length(id) < 1 | anyNA(id)) {
                print("Invalid sensor ID")
                return
        }
        
        ##check to make sure directory is valid and set wd
        tempDir <- paste(getwd(),.Platform$file.sep,directory,.Platform$file.sep,sep="")
        
        
        ##for each file
        ##count the number of non-NA rows and append to previous count
        ##sum the column for of non-NA rows and append to previous count

        pollutantSum <- 0
        pollutantCount <- 0
        pollutantMean <- NA
        for (i in seq_along(id)) {
                tempFrame <- NA
                templist <- NA
                fileName <- paste(tempDir,formatC(id[i],width = 3,flag = 0),".csv",sep="")
                try(tempFrame <- read.csv(fileName, na.strings=c("NA", "NULL")))
                try(tempList <- tempFrame[!is.na(tempFrame[pollutant]),pollutant])
                pollutantSum = pollutantSum + sum(tempList)
                pollutantCount = pollutantCount + length(tempList)
        } 
        pollutantMean = pollutantSum / pollutantCount
        print(pollutantMean)
        
}