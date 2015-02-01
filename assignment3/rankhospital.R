rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
    
    ## Check that state and outcome are valid
    state_name <- data$State
    problem <- c("heart attack", "heart failure", "pneumonia")
    right_hospital <- state_name == state
    right_outcome <- problem == outcome
    if(sum(right_hospital)== 0){
        stop("invalid state")
    }
    else if(sum(right_outcome)==0){
        stop("invalid outcome")
    }
    ## Return hospital name in that state with the given rank
    need_data <- data[right_hospital,]
    
    if(outcome == "heart attack"){
        x <- 11
    }
    else if(outcome == "heart failure"){
        x <- 17
    }
    else{
        x <- 23
    }
    need_data[,x] <- as.numeric(need_data[,x])
    ## 30-day death rate
    if(num == "best"){
        location = which.min(need_data[,x])
        need_data[,2][location]
    }
    else if(num == "worst"){
        location = which.max(need_data[,x])
        need_data[,2][location]
    }
    else if(num > length(need_data[,2])){
        NA
    }
    else{
        output <- need_data[order(need_data[,x],need_data[,2],na.last=NA),][,2][num]
        output

    }
}