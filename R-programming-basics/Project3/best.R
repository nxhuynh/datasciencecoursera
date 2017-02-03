# best() - find the best hospital given an outcome
# INPUTS:
# * state: 2-letter abbreviation of a state
# * outcome: an outcome name
# * (implicit) outcome-of-care-measures.csv
# OUTPUTS:
# * character vector of hospital name with the lowest mortality rate for the 
#   specified outcome
best <- function(state, outcome){
    ## Read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", na.strings = "Not Available")
 
    ## Check that state and outcome are valid
    if (!(state %in% data$State))
        stop("invalid state")
    else if (!(outcome %in% c("heart attack", "heart failure", "pneumonia")))
        stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death rate
    ## 11. Mortality rate from heart attack
    ## 17. Mortality rate from heart failure
    ## 23. Mortality rate from pneumonia
    if (outcome == "heart attack") col_id <- 11
    else if (outcome == "heart failure") col_id <- 17
    else col_id <- 23
 
    data[data == "Not Available"] = NA
    d <- data[data$State == state,]
    d <- d[complete.cases(d[,eval(col_id)]),]
    min_rate <- min(d[,eval(col_id)])
    candidates <- character(0)
    names <- as.character(d$Hospital.Name)
    for (i in 1:nrow(d)){
        if (d[i, col_id] == min_rate)
            candidates <- c(candidates, names[i])
    }
    message(candidates)
    min(candidates)
}