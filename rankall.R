## Return a data frame containing the hospitals with the given rank 30-day death rate for all states
##      state = two-letter code of state to search
##      outcome = name of condition of interest: “heart attack”, “heart failure”, or “pneumonia”
## Hospitals with no data on given outcome are excluded
## Data frame returned contains 2 columns: hospital (hospital name) and state (state abbr)
## If a state does not have a hospital at num rank for this outcome, then hospital is NA
## Text input is not case sensitive
rankall <- function(outcome, num = "best") {
    ## Read outcome data
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
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
    
    ## Translate best/worst
    if (tolower(num) == "best") num <- 1
    else if (tolower(num) == "worst") num <- -1 # flag to find worst
    else if (!is.numeric(num) || num <= 0) stop("invalid num")
    ## else num is the value as given
    
    ## Get the column name for this outcome
    col.name <- sprintf("Hospital.30.Day.Death..Mortality..Rates.from.%s", oc)
    
    ## Get the list of states
    states <- sort(unique(df[,"State"]))
    
    ## Set an empty data frame to collect hospitals
    rank.df <- data.frame(hospital = character(), state = character())
    
    ## Process each state
    for (curr.state in states) {
        
        ## Get hospitals in state
        ## Convert the column data to numeric values
        state.list <- subset(df, df$State == curr.state)
        state.list[, col.name] <- suppressWarnings(as.numeric(state.list[, col.name]))
        
        ## Sort the data by outcome column (INC), then by hospital name (INC)
        ## Remove any NA values
        rank.order <- order(state.list[, col.name], state.list[, "Hospital.Name"], na.last = NA)
        
        ## Get hospital names that match the min rate
        hosp.name <- NA
        
        ## Check rank if looking for worst (see above), else use num
        if(num < 0) rank.num <- length(rank.order) else rank.num <- num
        
        ## If there are enough rated hospitals
        if(rank.num <= length(rank.order)) 
            hosp.name <- state.list[rank.order,][rank.num, "Hospital.Name"]
        
        ## Add hospital and state to the data frame
        rank.df <- rbind(rank.df, data.frame(hospital = hosp.name, state = curr.state))
    }
    rank.df
}