complete <- function(directory, id = 1:332){
    
    a <- 1:length(id)
    b <- 1:length(id)
    retorno <- data.frame(a,b)
    
    j <- 1
    for(i in id)
    {
        fileName <- paste (".", "\\" ,directory, "\\" , formatC(i, 2, flag = "0") ,".csv", sep = "")
        partialData <- read.csv(fileName)
        cantidad <- nrow(na.exclude(partialData))
        retorno[j, 1] <- i
        retorno[j, 2] <- cantidad
        j <- j + 1
       
    }
    
    names(retorno) <- c("id", "nobs")
    retorno
}