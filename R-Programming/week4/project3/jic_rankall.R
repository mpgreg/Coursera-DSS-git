
rankall <- function(outcome, num = "best") {
        
        ##debugging variables
        ## outcome<-"heart failure"
        ## num <-5
        
        
        ##Set generic
        stateNames <- c(state.abb, "DC", "GU", "MP", "PR", "VI")
        orderedStateNames <- stateNames[order(stateNames)]
        returnData <- "Hospital.Name"
        returnDataFrame <- data.frame(hospital = NA, state = orderedStateNames, row.names = orderedStateNames)
        subsetBy <- "State"
        dataFile <- "outcome-of-care-measures.csv"
        
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
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the (abbreviated) state name
        
        allStatesOrdered <- data[order(data[,subsetBy], data[,codedOutcome], data[,returnData],na.last=NA), c(returnData,subsetBy,codedOutcome)]
        orderedSplit <- split(allStatesOrdered,as.factor(allStatesOrdered[[subsetBy]]))
        
        for(i in stateNames) {
                print(i)
                print(nrow(orderedSplit[[i]]))
                
                ##returnDataFrame[i,] <- c(i,nrow(orderedSplit[[i]]))
                
                if(num == "worst") {
                        ##length for this state
                        ##build data frame with each states worst
                        
                        ##stateDataOrdered[nrow(stateDataOrdered),returnData]
                   
                
                }
                
                if(num == "best") num<-1
                
                if(is.numeric(num)) {
                        ##build data frame with num(th) entry for each list element
                        
                        
                        ##if(num > nrow(stateData)) return(NA)
                        ##if(num > nrow(stateDataOrdered)) stop("num is int > num reporting hospitals")
                        ##if(num > 0 && num <= nrow(stateDataOrdered)) return(stateDataOrdered[num,returnData])
                }
                
                
        }
        
        return(returnDataFrame)
        
        
        
        ##this way ends up with just an ordered list of indices... loses hosp name.
        ##splitByState <- split(data, data[[subsetBy]])
        ##splitOrdered <- lapply(splitByState, function(y) order(y[,codedOutcome], y[,returnData]))
        ##lapply(splitByState, function(y) splitByState[[y]]        
        ##x$AK[lapply(x, function(y) order(y[,codedOutcome], y[,"Hospital.Name"]))$AK,c("Hospital.Name",codedOutcome)]
        ##x$TX[order(x$TX[,codedOutcome],x$TX[,"Hospital.Name"]),c("Hospital.Name","State",codedOutcome)]
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        ##stateData <- data[data$State==state,]
        ##stateDataOrdered <- stateData[order(stateData[,codedOutcome],stateData[,returnData],na.last=NA),c(returnData,codedOutcome)]
        
        
}
