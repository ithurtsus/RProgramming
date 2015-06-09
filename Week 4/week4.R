best <- function(state, outcome) {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings=c("Not Available"))
  
  ## Check that state and outcome are valid
  if (!(state%in%df[,7])) {
    stop("invalid state") 
  }
  
  if (!(outcome == "heart attack" || 
      outcome == "heart failure" || 
      outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  # Filter DF
  df <- df[df$State == state, ]
  df <- df[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  df[, 2] <- as.numeric(df[, 2])
  df[, 3] <- as.numeric(df[, 3])
  df[, 4] <- as.numeric(df[, 4])
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  if (outcome == "heart attack") {
    df <- df[order(df[2], df[1]),]
  }
  else if (outcome == "heart failure") {
    df <- df[order(df[3], df[1]),]
  } 
  else if (outcome == "pneumonia") {
    df <- df[order(df[4], df[1]),]
  }
  
  df$Hospital.Name[1]
}


rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings=c("Not Available"))
  
  ## Check that state and outcome are valid
  if (!(state%in%df[,7])) {
    stop('invalid state') 
  }
  
  if (!(outcome == "heart attack" || 
          outcome == "heart failure" || 
          outcome == "pneumonia")) {
    stop("invalid outcome")
  }
  
  # Filter DF
  df <- df[df$State == state, ]
  df <- df[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
  df[, 2] <- as.numeric(df[, 2])
  df[, 3] <- as.numeric(df[, 3])
  df[, 4] <- as.numeric(df[, 4])
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (outcome == "heart attack") {
    df <- df[order(df[2], df[1]),]
    df <- df[!is.na(df[2]),]
  }
  else if (outcome == "heart failure") {
    df <- df[order(df[3], df[1]),]
    df <- df[!is.na(df[3]),]
  } 
  else if (outcome == "pneumonia") {
    df <- df[order(df[4], df[1]),]
    df <- df[!is.na(df[4]),]
  }
  
  nr <- nrow(df)
  
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = nr
  } else if (num > nr) {
    return (NA)
  }
    
  df$Hospital.Name[num]
}

rankall <- function(outcome, num = "best") {  
  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE, na.strings=c("Not Available"))
  
  ## Check that state and outcome are valid  
  if (!(outcome == "heart attack" || 
          outcome == "heart failure" || 
          outcome == "pneumonia")) {
    stop("invalid outcome")
  }  
  
  state <- unique(df$State)
  hospital <- c()
  
  # Filter DF
  df <- df[c("Hospital.Name", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia", "State")]
  df[, 2] <- as.numeric(df[, 2])
  df[, 3] <- as.numeric(df[, 3])
  df[, 4] <- as.numeric(df[, 4])
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  if (outcome == "heart attack") {
    df <- df[order(df[2], df[1]),]
    df <- df[!is.na(df[2]),]
  }
  else if (outcome == "heart failure") {
    df <- df[order(df[3], df[1]),]
    df <- df[!is.na(df[3]),]
  } 
  else if (outcome == "pneumonia") {
    df <- df[order(df[4], df[1]),]
    df <- df[!is.na(df[4]),]
  }
  
  for (i in state) {
    hospital <- c(hospital, subfunction(df, i, outcome, num))
  }
  
  returndf <- data.frame(hospital, state, stringsAsFactors=FALSE)
  returndf[order(returndf[2]),]
}

subfunction <- function(df, state, outcome, num) {
  subdf <- df[df$State == state, ]
  nr <- nrow(subdf)
  
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = nr
  } else if (num > nr) {
    return (NA)
  }
  
  subdf$Hospital.Name[num]
}