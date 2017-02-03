pollutantmean <- function(directory, pollutant, id=1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate"
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: do not round the result!
 
    data <- NULL
       
    for (i in id){
        rawdata <- read.csv(paste0(directory,"/",sprintf("%03d",i),".csv"))
        temp <- rawdata[pollutant]
        data <- c(data, temp[!is.na(temp)])
    }
    
    mean(data)
}

#-------------------------------------------------------------------------------

complete <- function(directory, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    result <- data.frame(id=numeric(), nobs=numeric())
    
    for (i in id){
        data <- read.csv(paste0(directory,"/",sprintf("%03d",i),".csv"))
        n <- sum(complete.cases(data))
        result[nrow(result)+1,] <- c(i,n)
    }
    
    result
}

#-------------------------------------------------------------------------------

corr <- function(directory, threshold = 0){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    result <- NULL
    
    for (i in 1:332){
        if (complete(directory, i)$nobs[[1]] > threshold){
            data <- read.csv(paste0(directory,"/",sprintf("%03d",i),".csv"))
            dsulfate <- data$sulfate[complete.cases(data)]
            dnitrate <- data$nitrate[complete.cases(data)]
            result <- c(result, cor(dsulfate, dnitrate))
        }
    }
    
    result
}