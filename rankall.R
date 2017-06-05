rankall <- function(outcome, num = "best"){
    # Read outcomes data
    data <- read.csv("ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors = FALSE)
    
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
    
    # List of all available states
    states <- sort(unique(data$State))
    rank <- data.frame("hospital" = character())
    
    for (state in states) {
        # Be sure element of outcome are all numeric(char one will be coerced to mussing after this)    
        data[, outcomeIndex] <- suppressWarnings(as.numeric(data[, outcomeIndex]))
        
        # Just get data one state
        fullData <- data[data$State == state, ]
        # First order data by outcome values and then by name of hospital
        orderData <- fullData[order(fullData[, outcomeIndex], fullData[, 2]), ]
        
        # Now we remove all states with missing values
        orderData <- orderData[!is.na(orderData[, outcomeIndex]), ]
        
        val <- NA
        if(is.character(num)){
            # The best has the lower outcome, the first one
            if(num == "best"){
                val <- orderData[1, 2]
            }else if(num == "worst"){
                # The worst has the greater outcome, the last one
                val <- orderData[nrow(orderData), 2]
            }
        }else if(is.numeric(num)){
            # Here we just choose the one specified by num
            val <- orderData[num, 2]
        }
        
        # The new item to ad on the ranking data frame
        newDataSet <- data.frame("hospital" = val)
        rank <- rbind(rank, newDataSet)
    }
    # Now we add the state column as it is sorted first up there, it will match with items
    rank <- cbind(rank, state = states)
}