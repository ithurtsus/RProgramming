complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  nobs <- numeric()
  
  for (x in id) {
    currentCount <- 0
    currentDataFrame <- concatFiles(directory, x)
    for (y in 1:nrow(currentDataFrame)) {
      if (!is.na(currentDataFrame$sulfate[y]) && !is.na(currentDataFrame$nitrate[y])) {
        currentCount <- currentCount + 1
      }
    }
    nobs <- c(nobs, currentCount)
  }
  
  data.frame(id, nobs)
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