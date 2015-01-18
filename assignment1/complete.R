complete <- function(directory, id = 1:332) {
    name <- list.files(directory)
    thepath<-paste(directory,'\\',name[id],sep="")
    k <- 1
    for(i in thepath){
        data <- read.csv(i)
        complete <-na.omit(data)
        num_com <- length(complete[,1])
        num <- id[k]
        if(k == 1){
            output <- data.frame(id=num,nobs=num_com)
        }
        else{
            output <- rbind(output,c(num,num_com))
        }
        k <- k+1
    }
    output
}