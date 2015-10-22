complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        tempDir <- paste(getwd(),.Platform$file.sep,directory,.Platform$file.sep,sep="")
        
        ##might need to read actual files in the directory instead of assuming width=3
        ##tempFiles <- list.files(path=tempDir, pattern="*.csv", full.names=T, recursive=FALSE)
        
        returnFrame <- data.frame(id=NA,nobs=NA)
        
        for (i in seq_along(id)) {
                tempFrame <- NA
                fileName <- paste(tempDir,formatC(id[i],width = 3,flag = 0),".csv",sep="")
                try(tempFrame <- read.csv(fileName, na.strings=c("NA", "NULL")))
                try(completeRows <- sum(complete.cases(tempFrame)))
                returnFrame[i,1] = id[i]
                returnFrame[i,2] = completeRows
        } 
        
        print(returnFrame)       
}