rankall <- function(outcome, num = "best") {
    ## Reading the csv and storing it in a data frame
    data <- read.csv("outcome-of-care-measures.csv")
    
    ## creating a list of all the states without repetition
    state_l <- data[, "State"]
    state_l <- unique(state_l)
    state_l <- sort(state_l)
    ##creating a list of outcome to check valid outcome in args
    outcome_l <- c("Heart.Attack", "Heart.Failure", "Pneumonia")
    
    #checking if outcome in arg is valid
    if(outcome %in% outcome_l){
        ## creating empty data frame to store the rank from all the state
        rankdf <- data.frame()
        
        outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", outcome,
                         sep = "")
        for(state in state_l){
          temp_name <- data[data$State == state, "Hospital.Name"]
          suppressWarnings(temp_outr <- as.numeric(data[data$State == state,
                                                        outcome]))
          state_rank <- data.frame(Name = temp_name, Rate = temp_outr, 
                                   state=state)
          state_rank <- state_rank[complete.cases(state_rank$Rate),]
          attach(state_rank)
          state_rank <- state_rank[order(state_rank$Rate, state_rank$Name),]
          detach(state_rank)
          if(num == "best"){
              rankdf <- rbind(rankdf, state_rank[1,])
          }else if(num == "worst"){
              rankdf <- rbind(rankdf, state_rank[nrow(state_rank),])
          }else{
              rankdf <- rbind(rankdf, state_rank[num,])
          }
        }
        return(rankdf)
    }else{
        stop("invalid outcome")
    }
}