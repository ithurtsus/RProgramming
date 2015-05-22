corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  returnVector <- numeric()
  
  compset <- complete(directory)
  compsett <- compset$nobs > threshold
  
  for (x in compset$id[compsett]) {
    currentDataFrame <- concatFiles(directory, x)
    returnVector <- c(returnVector, cor(currentDataFrame$sulfate, currentDataFrame$nitrate, use = "pairwise.complete.obs"))    
  }
  
  returnVector
}

concatFiles <- function(directory, range) {
  for (x in range) {
    loc <- paste(directory, "/", formatC(x, width = 3, flag = "0"), ".csv", sep="")
    nextdf <- read.csv(loc)
    if (!exists("olddf")) {
      olddf <- nextdf
    } else {
      olddf <- rbind(olddf, nextdf)
    }
  }
  
  olddf
}