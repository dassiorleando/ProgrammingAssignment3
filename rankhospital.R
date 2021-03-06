rankhospital <- function(state, outcome, num = "best"){
    # Read outcomes data
    data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
    
    ## Check that state and outcome are valid
    # If this the state does not exists, we do this
    if(!(state %in% data$State)){
        stop("Invalid state")
    }
    
    # If the outcome column does not exist stop the program too
    possibleOutcomes <- c("heart attack", "heart failure", "pneumonia")
    if(!outcome %in% possibleOutcomes){
        stop("Invalid outcome")
    }
    
    
    # Get the corresponding outcome's index
    outcomeIndex <- NULL
    if(outcome == possibleOutcomes[1]){
        outcomeIndex <- 11
    }else if(outcome == possibleOutcomes[2]){
        outcomeIndex <- 17
    } else {
        outcomeIndex <- 23
    }

    # Be sure element of outcome are all numeric(char one will be coerced to mussing after this)    
    data[, outcomeIndex] <- suppressWarnings(as.numeric(data[, outcomeIndex]))

    # Just get data one state
    fullData <- data[data$State == state, ]
    # First order data by outcome values and then by name of hospital
    orderData <- fullData[order(fullData[, outcomeIndex], fullData[, 2]), ]
    
    # Now we remove all states with missing values
    orderData <- orderData[!is.na(orderData[, outcomeIndex]), ]
    
    if(is.character(num)){
        # The best has the lower outcome, the first one
        if(num == "best"){
            orderData[1, 2]
        }else if(num == "worst"){
            # The worst has the greater outcome, the last one
            orderData[nrow(orderData), 2]
        }
    }else if(is.numeric(num)){
        # Here we just choose the one specified by num
        orderData[num, 2]
    }
}