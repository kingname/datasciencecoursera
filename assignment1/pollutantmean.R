pollutantmean <- function(directory, pollutant, id = 1:332) {
    mydata <- c()
    name <- list.files(directory)
    thepath<-paste(directory,'\\',name[id],sep="")
    for(i in thepath){
        data <- read.csv(i)
        mydata <- c(mydata,data[pollutant][,1])
    }
    m <- mean(mydata,na.rm = TRUE)
}