rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that exists almost one state valid
    validStates <- table$State
    if(!match(state, validStates, nomatch = FALSE))
        stop("invalid state")
    
    ## Check if the outcome is valid
    outcomesCols <- matrix(c("heart attack", "heart failure", "pneumonia",  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), 3, 2)
    validOutcomes <- outcomesCols[,1]
    outcomeIndex <- match(outcome, validOutcomes, nomatch = FALSE)
    if(!outcomeIndex)
        stop("invalid outcome")
    outcomeColName <- outcomesCols[outcomeIndex, 2]
    
    ## Filter for the state info
    table <- table[table$State == state, ]
    
    ## Convert especific column outcome into numeric
    table[, outcomeColName] <- as.numeric(table[, outcomeColName])
    
    ## Filter NA values
    table <- table[complete.cases(table[outcomeColName]), ]
    
    ## order by outcome and hospital name
    table <- table[order(table[outcomeColName], table$Hospital.Name),]
    
    ## Return hospital name in that state with num 30-day death rate
    if(num == "best")
        num <- 1
    
    if(num == "worst")
        num <- nrow(table)
    
    table$Hospital.Name[num]
}