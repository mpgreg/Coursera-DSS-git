##This function takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), 
## and the ranking of a hospital in that state for that outcome (num). The function reads the outcome-of-care-measures.csv 
## file and returns a character vector with the name of the hospital that has the ranking specified by the num argument. 


## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               



rankhospital <- function(state, outcome, num = "best") {
        
        ##debugging variables
        ## state<-"TX"
        ## outcome<-"heart failure"
        ## num <-5
        
        
        ##Set desired return data
        returnData <- "Hospital.Name"
        dataFile <- "outcome-of-care-measures.csv"
        
        ## Check that state and outcome are valid
        stateNames <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
        
        if(is.null(state) || !state %in% stateNames) {
                stop("invalid state")
        }
        
        ## Check provided "outcome" to be one of “heart attack”, “heart failure”, or “pneumonia” and
        ## set to correct column name from dataset
        validOutcomes <- c("heart attack", "heart failure", "pneumonia")
        
        codedOutcome <- NULL
        if(is.null(outcome) || !outcome %in% validOutcomes) {
                stop("invalid outcome")
        }
        else {
                codedOutcome <- switch(outcome, 
                                       "heart attack" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", 
                                       "heart failure" = "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                                       "pneumonia" = "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        }
        
        ##Do initial check on rank number to make sure its valid.
        ##Later need to set valid range and check again.

        if(is.null(num) || !(is.numeric(num) || is.character(num))) stop("num is null or invalid data type")
        if(is.character(num) && !num %in% c("best","worst")) stop("num is invalid rank name")
        if(is.numeric(num) && num <= 0) stop("num is invalid rank number")
        
        
        ## Read outcome data
        data <- read.csv(dataFile, colClasses = "character")
        suppressWarnings(data[,codedOutcome] <- as.numeric(data[, codedOutcome]))
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        stateData <- data[data$State==state,]
        stateDataOrdered <- stateData[order(stateData[,codedOutcome],stateData[,returnData],na.last=NA),c(returnData,codedOutcome)]
        
        if(is.character(num)) switch(num,"best" = return(stateDataOrdered[1,returnData]),
                                         "worst" = return(stateDataOrdered[nrow(stateDataOrdered),returnData]))
        if(is.numeric(num)) {
                if(num > nrow(stateData)) return(NA)
                if(num > nrow(stateDataOrdered)) stop("num is int > num reporting hospitals")
                if(num > 0 && num <= nrow(stateDataOrdered)) return(stateDataOrdered[num,returnData])
        }
}