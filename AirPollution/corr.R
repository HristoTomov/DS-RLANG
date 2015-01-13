corr <- function(directory, threshold = 0) {
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        files <- list.files(directory, full.names=TRUE)
        data <- lapply(files, function(i) {na.omit(read.csv(i, header=TRUE))})
        result <- sapply(data, function(i) { NROW(i) })        
        id <-1:NROW(files)
        nobs <- cbind(id, result)
        selected <- subset(nobs, result > threshold)
        data <- data[selected[,"id"]]
        sapply(data, function(i) cor(i$nitrate, i$sulfate))
}
