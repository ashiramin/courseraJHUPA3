rankhospital <- function(state, disease, num = "best") {
  ## Read outcome data
  options(warn=-1)
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcome[, 11] <- as.numeric(outcome[, 11]) # Heart Attack
  outcome[, 17] <- as.numeric(outcome[, 17]) # Heart Failure
  outcome[, 23] <- as.numeric(outcome[, 23]) # pneumonia
  
  diseases <- c("pneumonia", "heart failure", "heart attack")
  
  if (!any(outcome$State==state,na.rm = FALSE))
  {
    stop("No such State exists")
  }
  
  if (any(outcome %in% diseases))
  {
    stop("No such disease exists")
  }
  
  if (disease == "pneumonia") {
    hospitals <- outcome[outcome$State==state,] 
    hospitals <- hospitals[order(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),] 
    hospitalRank <- cbind(hospitals$Hospital.Name
                          ,hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
                          ,1:nrow(hospitals))
    colnames(hospitalRank) <- c("Hospital.Name", "Rate", "Rank")
    row.names(hospitalRank) <- 1:nrow(hospitals)
  }
  else if (disease == "heart failure") {
    hospitals <- outcome[outcome$State==state,] 
    hospitals <- hospitals[order(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),] 
    hospitalRank <- cbind(hospitals$Hospital.Name
                          ,hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
                          ,1:nrow(hospitals))
    colnames(hospitalRank) <- c("Hospital.Name", "Rate", "Rank")
    row.names(hospitalRank) <- 1:nrow(hospitals)
  }
  else if (disease == "heart attack") {
  
    hospitals <- outcome[outcome$State==state,] 
    hospitals <- hospitals[order(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),] 
    hospitalRank <- cbind(hospitals$Hospital.Name
                          ,hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
                          ,1:nrow(hospitals))
    colnames(hospitalRank) <- c("Hospital.Name", "Rate", "Rank")
    row.names(hospitalRank) <- 1:nrow(hospitals)
    
  }
  return(hospitalRank)
  
  
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
}