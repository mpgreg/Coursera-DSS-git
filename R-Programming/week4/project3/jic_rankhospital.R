##This function takes three arguments: the 2-character abbreviated name of a state (state), an outcome (outcome), 
## and the ranking of a hospital in that state for that outcome (num). The function reads the outcome-of-care-measures.csv 
## file and returns a character vector with the name of the hospital that has the ranking specified by the num argument. 


## [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
## [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
## [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               



rankhospital <- function(state, outcome, num = "best") {
        
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
        ##Later need to update valid range and check again.
        possibleRanks <- 1:length(stateNames)
        print(num)
        if(is.null(num)) stop("num is null")
        if(is.numeric(num) && !num %in% possibleRanks) stop("num is int > 55")
        if(is.character(num) && !(num == "best" || num== "worst")) stop("num is invalid char")
        
        
        
        ## Read outcome data
        data <- read.csv(dataFile, colClasses = "character")
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        suppressWarnings(data[,codedOutcome] <- as.numeric(data[, codedOutcome]))
        
        stateData <- data[data$State==state,]
        
        stateData[order(stateData[,codedOutcome],stateData[,"Hospital.Name"],na.last=NA),c("Hospital.Name",codedOutcome)]
        

        
        ##codedMin <- min(stateData[[codedOutcome]],na.rm=TRUE)
        ##bestHospitalList <- stateData[which(stateData[[codedOutcome]] == codedMin), returnData ]
        ##bestHospitalList[sort.list(bestHospitalList)[1]]
        
        
        ##Calculate valid range for rank number and check num var.
        
        ##
        
        ##checkRank()
        
        
}

checkRank <- function(ranks,num){
        
        
        
}