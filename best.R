best <- function(state, outcome){
  data <- read.csv("outcome-of-care-measures.csv")
  state_l <- data[, "State"]
  state <- unique(state)
  outcome_l <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
  #checking if the state in arg in valid
  if(state %in% state_l){ 
    #checking if outcome in arg is valid
    if(outcome %in% outcome_l){
      outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome,
                       sep = "")
      ## out_data contains hospital located in the state mentioned in arg
      out_data <- data[c(data$State == state),c("Hospital.Name", outcome)]
      ##transforming outcome coln from chr to num
      out_data <- transform(out_data, out = as.numeric(out_data[, outcome]))
      ##removing the missing data aka cleaning data
      out_data <- out_data[complete.cases(out_data),]
      ##retrving hospital name with least mortality rate
      hosp <- out_data$Hospital.Name[which.min(out_data[,"out"])]
      return(hosp)
    }else{
      stop("invalid outcome")
    }
  }else{
    stop("invalid state")
  }
}