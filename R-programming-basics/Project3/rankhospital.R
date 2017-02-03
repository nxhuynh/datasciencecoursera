# rankhospital() - find the hospital for a given state, disease, and rank
# INPUTS:
# * state: 2-letter abbreviation of a state name
# * outcome: the disease of interest
# * num: rank (can be numeric / "best" / "worst")
# OUTPUT:
# * character vector with hospital name that ranks nth in the state
#   for the given disease, 
#   + if num > # hospitals in state, returns NA
#   + if tied, returns the hospital whose name comes first in alphabetic order

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", 
                        na.strings = "Not Available")
    ## Check that state and outcome are valid
    if (!(state %in% data$State)) stop("invalid state")
    if (outcome == "heart attack") outcome_id <- 11
    else if (outcome == "heart failure") outcome_id <- 17
    else if (outcome == "pneumonia") outcome_id <- 23
    else stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    d <- data[data$State == state & complete.cases(data[,eval(outcome_id)]),]
    if (num == "best") num <- 1
    else if (num == "worst") num <- nrow(d)
    ret <- NA
    if (num <= nrow(d)){
        d1 <- data.frame("Hospitals" = as.character(d$Hospital.Name), 
                         "Rates" = as.numeric(d[,eval(outcome_id)]))
        # this method can't break tie using another column as required by the assignment
        # d2 <- transform(d1, Rank = ave(d1$Rates, FUN = function(x) rank(x, ties.method = "min")))
        # so use order() instead
        d2 <- d1[order(d1$Rates, d1$Hospitals),]
        ret <- d2$Hospitals[num]
    }
    ret
}