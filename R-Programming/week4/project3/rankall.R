## This function takes two arguments: an outcome name (outcome) and a hospital rank- ing (num). The function reads the 
## outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital in each state that has the 
## ranking specified in num. For example the function call rankall("heart attack", "best") would return a data frame containing 
## the names of the hospitals that are the best in their respective states for 30-day heart attack death rates.

rankall <- function(outcome, num = "best") {
        
        ##Set generic vars
        stateNames <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
        orderedStateNames <- stateNames[order(stateNames)]
        returnData <- "Hospital.Name"
        returnDataFrame <- data.frame(hospital = NA, state = orderedStateNames, row.names = orderedStateNames)
        subsetBy <- "State"
        dataFile <- "outcome-of-care-measures.csv"
        returnRank <- NULL
        
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
        if(is.numeric(num) && (num <= 0 || length(num) > 1)) stop("num is invalid rank number")
        
        
        ## Read outcome data
        data <- read.csv(dataFile, colClasses = "character")
        suppressWarnings(data[,codedOutcome] <- as.numeric(data[, codedOutcome]))
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the (abbreviated) state name
        orderFactor <- order(data[,subsetBy], data[,codedOutcome], data[,returnData],na.last=NA)
        allStatesOrdered <- data[orderFactor, c(returnData,subsetBy,codedOutcome)]
        orderedSplit <- split(allStatesOrdered,as.factor(allStatesOrdered[[subsetBy]]))
        
        for(i in names(orderedSplit)) {
                
                if(num == "best") returnRank <- 1
                if(num == "worst") returnRank <- nrow(orderedSplit[[i]])
                if(is.numeric(num)) returnRank <- num
                
                returnDataFrame[i,1] <- orderedSplit[[i]][returnRank,returnData]
        }
        
        return(returnDataFrame)
}
