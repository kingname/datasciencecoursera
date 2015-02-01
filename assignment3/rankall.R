rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    ## Check that outcome are valid
    problem <- c("heart attack", "heart failure", "pneumonia")
    right_outcome <- problem == outcome
    if(sum(right_outcome)==0){
        stop("invalid outcome")
    }
    state_list <- unique(data$State)
    ## For each state, find the hospital of the given rank
    state_vector <- c()
    hospital_vector <- c()
    for(each in state_list){
        hospital_name = rankhospital(each,outcome,num)
        state_vector <- c(state_vector,each)
        hospital_vector <- c(hospital_vector,hospital_name)
    }
    ## Return a data frame with the hospital names and the
    output <- data.frame(hospital=hospital_vector,state=state_vector)
    ## (abbreviated) state name
    output
}
