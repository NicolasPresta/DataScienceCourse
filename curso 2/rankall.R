rankall <- function(outcome, num = "best") {
    ## Read outcome data
    table <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check if the outcome is valid
    outcomesCols <- matrix(c("heart attack", "heart failure", "pneumonia",  "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"), 3, 2)
    validOutcomes <- outcomesCols[,1]
    outcomeIndex <- match(outcome, validOutcomes, nomatch = FALSE)
    if(!outcomeIndex)
        stop("invalid outcome")
    outcomeColName <- outcomesCols[outcomeIndex, 2]
    
    ## Convert especific column outcome into numeric
    table[, outcomeColName] <- as.numeric(table[, outcomeColName])
    
    ## Filter NA values
    table <- table[complete.cases(table[outcomeColName]), ]
    
    ## order by outcome and hospital name
    table <- table[order(table[outcomeColName], table$Hospital.Name),]
    
    ## split data for state
    list <- split(table, table$State)
    
    ## format num parameter
    result <- sapply(list, rank, num)
    
    ## return
    as.data.frame(result)
}

rank <- function(data, num = "best") {
    
    if(num == "best")
        num <- 1
    
    if(num == "worst")
        num <- nrow(data)
    
    data$Hospital.Name[num]
}
