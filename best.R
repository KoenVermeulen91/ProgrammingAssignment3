
best <- function(state, outcome) {
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        options(warn = -1)
        outcome.df[, 11] <- as.numeric(outcome.df[, 11])
        outcome.df[, 17] <- as.numeric(outcome.df[, 17])
        outcome.df[, 23] <- as.numeric(outcome.df[, 23])
        
        #validate input values
        if (!(state %in% outcome.df$State)) stop("invalid state")
        if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
        
        # filter for state
        bestm <- dplyr::filter(outcome.df, outcome.df$State == state)
        # find hospital name with minimum rate (which.min)
        if (outcome == "heart attack") {
                return(bestm$Hospital.Name[which.min(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)])
        } else {
                if (outcome == "heart failure") {
                        return(bestm$Hospital.Name[which.min(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)])      
                } else {
                        if (outcome == "pneumonia") {
                                return(bestm$Hospital.Name[which.min(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)])
                        } else {
                                print("nee")
                        }
                }
        }
}

best("TX", "heart attack") # "CYPRESS FAIRBANKS MEDICAL CENTER"
best("TX", "heart failure") # "FORT DUNCAN MEDICAL CENTER"
best("MD", "heart attack") # "JOHNS HOPKINS HOSPITAL, THE"
best("MD", "pneumonia") # "GREATER BALTIMORE MEDICAL CENTER"
best("BB", "heart attack") # Error in best("BB", "heart attack") : invalid state
best("NY", "hert attack") #Error in best("NY", "hert attack") : invalid outcome
