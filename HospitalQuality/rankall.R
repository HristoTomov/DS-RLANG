rankall <- function(outcome, num = "best") {
        
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
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        i_outcome <- match(outcome, outcomes_list) + 2
        df <- df[c(1,2, i_outcome)]
        df[, 3] <- suppressWarnings(as.numeric(df[, 3]))
        df <- df[!is.na(df[,3]),]
        df <- df[order(df[3]),c(1,2)]
        colnames(df) <- c("hospital", "state")
        df <- split(df,df[2])
        if(num == "best") num <-1
        if(num == "worst") {
                df <- lapply(df, function(x) x[nrow(x),c(1,2)])
        } else {
                df <- lapply(df, function(x) x[num,c(1,2)])
        }
        df <- do.call(rbind, df)
}
## Output:
## > head(rankall("heart attack", 20), 10)
## hospital state
## AK                                <NA>  <NA>
## AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
## AR   ARKANSAS METHODIST MEDICAL CENTER    AR
## AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
## CA               SHERMAN OAKS HOSPITAL    CA
## CO            SKY RIDGE MEDICAL CENTER    CO
## CT          ROCKVILLE GENERAL HOSPITAL    CT
## DC                                <NA>  <NA>
## DE                                <NA>  <NA>
## FL                DOCTORS HOSPITAL INC    FL
## > tail(rankall("pneumonia", "worst"), 3)
## hospital state
## WI MAYO CLINIC HEALTH SYSTEM - NORTHLAND, INC    WI
## WV                     PLATEAU MEDICAL CENTER    WV
## WY           NORTH BIG HORN HOSPITAL DISTRICT    WY
## > tail(rankall("heart failure"), 10)
## hospital state
## TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
## TX                                        FORT DUNCAN MEDICAL CENTER    TX
## UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
## VA                                          SENTARA POTOMAC HOSPITAL    VA
## VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
## VT                                              SPRINGFIELD HOSPITAL    VT
## WA                                         HARBORVIEW MEDICAL CENTER    WA
## WI                                        WAUKESHA MEMORIAL HOSPITAL    WI
## WV                                         FAIRMONT GENERAL HOSPITAL    WV
## WY                                        CHEYENNE VA MEDICAL CENTER    WY
