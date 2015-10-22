## The function reads the outcome-of-care-measures.csv file and returns a character vector 
## with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified 
## outcome in that state. The hospital name is the name provided in the Hospital.Name variable. The 
## outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not 
## have data on a particular outcome should be excluded from the set of hospitals when deciding the 
## rankings.

## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               



best <- function(state, outcome) {

        ##Set desired return data
        returnData <- "Hospital.Name"
        
        ## Check that state and outcome are valid
        stateNames <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
        
        if(is.null(state) || !state %in% stateNames) {
                stop("invalide state")
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
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        suppressWarnings(data[,codedOutcome] <- as.numeric(data[, codedOutcome]))
        
        stateData <- data[data$State==state,]
        codedMin <- min(stateData[[codedOutcome]],na.rm=TRUE)
        bestHospitalList <- stateData[which(stateData[[codedOutcome]] == codedMin), returnData ]
        
        ##Now if there is a tie return the alphabetically first one.
        bestHospital <- bestHospitalList[sort.list(bestHospitalList)[1]]
        
        ##data[which.min(stateData[[codedOutcome]]),"Hospital.Name"]        
        ##stateMins <- tapply(data[[codedOutcome]],data$State,min,na.rm=TRUE)
        ##data[data$State==state,11]
        
}