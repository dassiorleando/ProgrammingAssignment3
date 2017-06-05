best <- function(state, outcome){
    ## Read outcomes data
    outcomes <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    # If this the state does not exists, we do this
    if(!(state %in% outcomes$State)){
        stop("Invalid state")
    }
    
    # If the outcome column does not exist stop the program too
    possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% possibleOutcomes){
        stop("Invalid outcome")
    }
    
    outcomeIndex <- NULL
    
    # Get the corresponding index outcome
    if(outcome == possibleOutcomes[1]){
        outcomeIndex <- 11
    }else if(outcome == possibleOutcomes[2]){
        outcomeIndex <- 17
    } else {
        outcomeIndex <- 23
    }
    
    # We select all data of the input outcome
    outcomeData <- as.numeric(outcomes[, outcomeIndex])
    # Delete missing data before proceding
    bad <- is.na(outcomeData)
    goodOutcomes <- outcomes[!bad,]
    
    # Fetch only those of the selected state
    # data to work with
    data <- goodOutcomes[goodOutcomes$State == state, ]
    # Entire col of valid data corresponding to the desired state
    outcomeCol <- as.numeric(data[, outcomeIndex])
    
    # Get the index of the row where we have the minimum outcome
    # It is also the index of the corresponding row in our data
    desired_row <- which(outcomeCol == min(outcomeCol))
    
    # Now we just select the Hostital name of the row
    data[desired_row, "Hospital.Name"]
}