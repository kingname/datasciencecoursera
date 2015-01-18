corr <- function(directory, threshold = 0) {
    name <- list.files(directory)
    comp <- complete(directory)
    output <- c()
    x <- comp['nobs']>threshold
    usefuldata <- as.vector(comp[x,])
    if(length(usefuldata$id) != 0){
        thepath<-paste(directory,'\\',name[as.vector(usefuldata$id)],sep="")
        for(i in thepath){
            data <- read.csv(i)
            output <- c(output,cor(na.omit(data)["sulfate"],na.omit(data)["nitrate"]))
        }
    }
    output
}