## Return hospital name in a state with lowest 30-day death rate
##      state = two-letter code of state to search
##      outcome = name of condition of interest: “heart attack”, “heart failure”, or “pneumonia”
## Hospitals with no data on given outcome are excluded
## Text input is not case sensitive
best <- function(state, outcome) {
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
    
    ## Get the minimum rate
    min.rate <- min(state.list[, col.name], na.rm = TRUE)
    
    ## Get hospital names that match the min rate
    hosp.list <- state.list[state.list[col.name] == min.rate & !is.na(state.list[col.name]), "Hospital.Name"]
    sort(hosp.list)
    hosp.list[1]
}