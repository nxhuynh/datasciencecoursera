# rankall() - find the hospitals in all states given a disease and rank
# INPUTS:
# * outcome: the disease of interest
# * num: the rank of interest
# OUTPUT:
# * 2-column data frame containing hospital names and state

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", na.strings = "Not Available")
    ## Check that state and outcome are valid
    if (outcome == "heart attack") outcome_id <- 11
    else if (outcome == "heart failure") outcome_id <- 17
    else if (outcome == "pneumonia") outcome_id <- 23
    else stop("invalid outcome")
    data <- data[complete.cases(data$State),]
    ## For each state, find the hospital of the given rank
    ret <- data.frame("hospital" = character(0), "state" = character(0), stringsAsFactors = FALSE)
    i <- 0
    for (state in sort(unique(data$State))){
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        d <- data[data$State == state & complete.cases(data[,eval(outcome_id)]),]
        if (num == "best") rank <- 1
        else if (num == "worst") rank <- nrow(d)
        else rank <- num
        name <- NA
        
        if (rank <= nrow(d)){
            d1 <- data.frame("Hospitals" = as.character(d$Hospital.Name), 
                             "Rates" = as.numeric(d[,eval(outcome_id)]))
            d2 <- d1[order(d1$Rates, d1$Hospitals),]
            name <- as.character(d2$Hospitals[rank])
        }
        i <- i+1
        ret[i,] <- list(name, as.character(state))
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    ret
}