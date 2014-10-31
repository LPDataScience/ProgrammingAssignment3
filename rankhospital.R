## Return hospital name in that state with the given rank 30-day death rate
##      state = two-letter code of state to search
##      outcome = name of condition of interest: “heart attack”, “heart failure”, or “pneumonia”
##      num = rank to return: integer, "best", or "worst"
## Hospitals with no data on given outcome are excluded
## If num exceeds the number of hospitals ranked, then NA is returned
## Text input is not case sensitive
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    state.list <- subset(df, df$State == toupper(state))
    if(nrow(state.list) == 0)
        stop("invalid state")
    
    ## Set outcome column text
    oc <- switch(tolower(outcome)
                 , "heart attack" = "Heart.Attack"
                 , "heart failure" = "Heart.Failure"
                 , "pneumonia" = "Pneumonia"
                 , "Not Found"
                 )
    ## Stop if no valid outcome was provided
    if (oc == "Not Found")
            stop("invalid outcome")
    
    ## Get the column name for this outcome
    col.name <- sprintf("Hospital.30.Day.Death..Mortality..Rates.from.%s", oc)
    
    ## Convert the column data to numeric values
    state.list[, col.name] <- suppressWarnings(as.numeric(state.list[, col.name]))
    
    ## Sort the data by outcome column (INC), then by hospital name (INC)
    ## Remove any NA values
    rank.order <- order(state.list[, col.name], state.list[, "Hospital.Name"], na.last = NA)
    
    ## Get hospital names that match the min rate
    hosp.name <- NA
    
    ## Translate best/worst
    if (num == "best") num <- 1
    else if (num == "worst") num <- length(rank.order)
    else if (!is.numeric(num)) stop("invalid num")
    ## else num is the value as given
    
    ## If there are enough rated hospitals
    if(num <= length(rank.order)) 
        hosp.name <- state.list[rank.order,][num, "Hospital.Name"]
    
    ## Return name
    hosp.name
}