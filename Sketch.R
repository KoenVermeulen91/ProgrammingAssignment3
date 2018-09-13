##Programming Assignment 3: Hospital Quality

#Reading Data
hospital.data <- read.csv("hospital-data.csv")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")


##Exploratory analysis Hospital data

#head
#summary
#str
#dim

#Address 2 & 3 are empty
sum(!is.na(hospital.data$Address.2))
sum(!is.na(hospital.data$Address.3))

#Frequency table hospital types
hospital.types <- table(hospital.data$Hospital.Type)

#Frequency table hospital ownership
hospital.ownership <- table(hospital.data$Hospital.Ownership)
#Some values aren't entered correctly (e.g. Government-Local & Government - Local exist)

#Frequency table Emergency services
emergency.services <- table(hospital.data$Emergency.Services)
#Binary, 23 rows NA

##Exploratory analysis Outcome Care data

#head
#summary
#str
#dim

#Assignment 1: Mortality for heart attack
head(outcome)
ncol(outcome)
names(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

#Assignment 2: Function best hospital in state
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
                return(bestm$Hospital.Name[which.min(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, rm.na = T)])
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

#Assignment 3: Ranking hospitals by outcome by state

rankhospital <- function(state, outcome, num) {
        #and the ranking of a hospital in that state for that outcome (num)
        outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        #options(warn = -1)
        outcome.df[, 11] <- as.numeric(outcome.df[, 11])
        outcome.df[, 17] <- as.numeric(outcome.df[, 17])
        outcome.df[, 23] <- as.numeric(outcome.df[, 23])
        
        #validate input values
        if (!(state %in% outcome.df$State)) stop("invalid state")
        if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome") 
        
        bestm <- dplyr::filter(outcome.df, outcome.df$State == state)
        #validate number of rows
        #if (num %in% c("best", "worst")
        if (num > nrow(bestm)) return(NA)
        
        if (outcome == "heart attack") {
                best.ha <- bestm[order(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, bestm$Hospital.Name), ]
                return(best.ha$Hospital.Name[num])
        } else {
                if (outcome == "heart failure") {
                        best.hf <- bestm[order(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, bestm$Hospital.Name), ]
                        return(best.hf$Hospital.Name[num])
                        #bestm$Hospital.Name[which.min(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)]      
                } else {
                        if (outcome == "pneumonia") {
                                best.p <- bestm[order(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, bestm$Hospital.Name), ]
                                return(best.p$Hospital.Name[num])
                                #bestm$Hospital.Name[which.min(bestm$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)]
                        } else {
                                print("nee")
                        }
                }
        }
        
        
        # The num argument can take values “best” (min!! or which.min), “worst” (max or which.max), 
        # or an integer indicating the ranking (smaller numbers are better).
        # order by outcome
}

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}


rankhospital("TX", "heart failure", 4) # "DETAR HOSPITAL NAVARRO"
rankhospital("MD", "heart attack", "worst") # "HARFORD MEMORIAL HOSPITAL"
rankhospital("MN", "heart attack", 5000) #NA





        


#head(set, number of rows)
#tail(set, number of rows)
#dim(set)
#class(set)
#summary(set)
#str(set) - structure of the set

#tidyr::drop_na(set) - drop rows with na value
#drop rows with all na value

#split(set, set$col1) - split the dataset by the values found in col1

#cls_list <- lapply(set, function)
##List of function applied to all variables
##Store as matrix (as.matrix(cls_list))
#table(set$col1) - frequency table

#range(set)
#unique(set)
#length(set)

#sapply(set, unique)

