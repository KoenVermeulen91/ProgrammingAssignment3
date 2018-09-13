
rankhospital <- function(state, outcome, num) {
        # Reading data    
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # Turning off warning messages
        options(warn = -1)
        
        # Transform outcome-columns to numeric
        outcome.df[, 11] <- as.numeric(outcome.df[, 11])
        outcome.df[, 17] <- as.numeric(outcome.df[, 17])
        outcome.df[, 23] <- as.numeric(outcome.df[, 23])
        
        # Validate input values
        if (!(state %in% outcome.df$State)) stop("invalid state")
        if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome") 
        
        # Select state
        bestm <- dplyr::filter(outcome.df, outcome.df$State == state)
        
        # num != "best" | "worst" | num < nrow
        if (outcome == "heart attack") {
                best.ha <- bestm[order(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, bestm$Hospital.Name), ]
                best.ha <- dplyr::select(best.ha, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                best.ha <- na.omit(best.ha)
                num <- ifelse(num == "best", 1, ifelse(num == "worst", nrow(best.ha), num))
                if (num > nrow(best.hf)) return(NA)
                return(best.ha$Hospital.Name[num])
        }
        if (outcome == "heart failure") {
                best.hf <- bestm[order(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, bestm$Hospital.Name), ]
                best.hf <- dplyr::select(best.hf, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                best.hf <- na.omit(best.hf)
                num <- ifelse(num == "best", 1, ifelse(num == "worst", nrow(best.hf), num))
                if (num > nrow(best.hf)) return(NA)
                return(best.hf$Hospital.Name[num])
        }
        if (outcome == "pneumonia") {
                best.p <- bestm[order(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, bestm$Hospital.Name), ]
                best.p <- dplyr::select(best.p, Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                best.p <- na.omit(best.p)
                num <- ifelse(num == "best", 1, ifelse(num == "worst", nrow(best.p), num))
                if (num > nrow(best.p)) return(NA)
                return(best.p$Hospital.Name[num])
        } 
}

rankhospital("TX", "heart failure", 4) # "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") # "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) #NA
