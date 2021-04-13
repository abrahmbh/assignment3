rankhospital <- function(state, outcome, num = "best"){
  data <- read.csv("outcome-of-care-measures.csv")
  state_l <- data[, "State"]
  state_l <- unique(state_l)
  outcome_l <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
  #checking if the state in arg in valid
  if(state %in% state_l){
    #checking if outcome in arg is valid
    if(outcome %in% outcome_l){
      outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome,
                       sep = "")
      ## subsetting data to get "Provider.Number", "Hospital.Name", "Rate",
      provider_num <- data[data$State==state, "Provider.Number"]
      hosp_name <- data[data$State==state, "Hospital.Name"]
      ## subsetting rates as numeric so its easy to sort later
      out_rates <- as.numeric(data[data$State==state, outcome])
      ## storing provider number, hospital name, rates in a df
      sort_data <- data.frame(Provider.Number = provider_num, Hospital.Name = 
                                hosp_name, Rates = out_rates)
      ## removing all the complete cases
      sort_data <- sort_data[complete.cases(sort_data),]
      
      ## sorting the data frame based on the "Rates" columm
      suppressWarnings(attach(sort_data))
      sort_data<-sort_data[order(sort_data$Rates, sort_data$Hospital.Name),]
      detach(sort_data)
      ## handling if num is best
      if(num == "best"){
        ## calling the best function should do this
        return(sort_data$Hospital.Name[1])
      }
      ## handiling if num is worse
      else if(num == "worst"){
        return(sort_data$Hospital.Name[nrow(sort_data)])
      }
      ## assuming anything else than best/worse is numeric
      else{
        if(num > nrow(sort_data)){
          
          return(NA)
        }else{
          return(sort_data$Hospital.Name[num])
        }
      }
    }else{
      stop("invalid outcome")
    }
  }else{
    stop("invalid state")
  }
}