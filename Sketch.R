##Programming Assignment 3: Hospital Quality

#Reading Data
hospital.data <- read.csv("hospital-data.csv")
outcome.care <- read.csv("outcome-of-care-measures.csv")

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

