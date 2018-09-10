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
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        outcome[, 11] <- as.numeric(outcome[, 11])
        outcome[, 17] <- as.numeric(outcome[, 17])
        outcome[, 23] <- as.numeric(outcome[, 23])
        
        if (state %in% outcome$State) stop("invalid state")
        if (outcome == "heart attack" | "heart failure" | "pneumonia" ) stop("invalid outcome")
        
        bestfilter1 <- dplyr::filter(outcome, outcome$State == state)
        bestfilter2 <- dplyr::select(bestfilter1, outcome)
        
        # input state and outcome, create matrix with names and outcomes, minimize outcome, print name
        
        
        }
        
        ## Read outcome data
        
        ## State
        
        ## Check validity of outcome ("invalid state")
        
        ## Outcomes can be "heart attack", "heart failure" & "pneumonia" (11, 17 & 23)
        
        ## Return hospital name in state with lowest 30-day death rate
        ## Hospital name is provided in outcome$Hospital.Name
        
        
}




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

