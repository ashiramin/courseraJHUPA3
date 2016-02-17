best <- function(state, disease) {
  
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
    minRate <- min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia[outcome$State == state]
                   ,na.rm = TRUE)
    name <- outcome$Hospital.Name[outcome
                                  $Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minRate 
                                  & outcome$State==state] 
  }
  else if (disease == "heart failure") {
    
    minRate <- min(outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure[outcome$State == state]
                   ,na.rm = TRUE)
    
    name <- outcome$Hospital.Name[outcome
                                  $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minRate 
                                  & outcome$State==state] 
  }
  else if (disease == "heart attack") {
    minRate <- min(outcome
                    $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack[outcome$State == state]
                   ,na.rm = TRUE)
    name <- outcome$Hospital.Name[outcome
                                  $Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minRate 
                                  & outcome$State==state] 
    
  }
  name <- na.omit(name)
  return(name)
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}