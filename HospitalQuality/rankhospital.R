rankhospital <- function(state, outcome, num = "best") {
        
        ## Check that outcome is valid        
        outcomes_list = c("heart attack", "heart failure", "pneumonia")
        if(!is.element(outcome, outcomes_list)) stop("invalid outcome")
        
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")[, c(2,7,11,17,23)]
        ## > names(df)
        ## [1] "Hospital.Name"                                             
        ## [2] "State"                                                     
        ## [3] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        ## [4] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        ## [5] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        
        ## Check that state is valid
        if(!is.element(state, unique(df[[2]]))) stop("invalid state")
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        i_outcome <- match(outcome, outcomes_list) + 2
        df <- df[df$State==state, c(1, i_outcome)]
        df[, 2] <- suppressWarnings(as.numeric(df[, 2]))
        df <- df[!is.na(df[,2]),]
        df <- df[order(df[2]),c(1,2)]
        if(num == "best") num <- 1
        if(num == "worst") num <- nrow(df)
        df[num,1]
}
## Output:
## > rankhospital("TX", "heart failure", 4)
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## > rankhospital("MD", "heart attack", "worst")
## [1] "HARFORD MEMORIAL HOSPITAL"
## > rankhospital("MN", "heart attack", 5000)
## [1] NA