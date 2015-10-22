corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        ## NOTE: Do not round the result!

        tempDir <- paste(getwd(),.Platform$file.sep,directory,sep="")
        tempFiles <- list.files(path=tempDir, pattern="*.csv", full.names=T, recursive=FALSE)
        returnVector <- numeric(length=0)
        
        for (instFile in tempFiles) {
                tempFrame <- NA
                completeFrame <- NA
                corrNS <- NA

                try(tempFrame <- read.csv(instFile, na.strings=c("NA", "NULL")))
                completeFrame <- tempFrame[complete.cases(tempFrame),]

                if(nrow(completeFrame) > threshold) {
                        corrNS <- cor(completeFrame[[2]],completeFrame[[3]])
                        returnVector <- append(returnVector,corrNS)
                }
        } 
        return(returnVector)




}