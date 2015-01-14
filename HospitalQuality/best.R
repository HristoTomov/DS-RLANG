best <- function(state, outcome) {
        
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        i_outcome <- match(outcome, outcomes_list) + 2
        df <- df[df$State==state, c(1, i_outcome)]
        df[, 2] <- suppressWarnings(as.numeric(df[, 2]))
        df <- df[!is.na(df[,2]),]
        df[which.min(df[,2]),1]
}
## Outcome:
## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state
## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome


