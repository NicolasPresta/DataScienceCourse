corr <- function(directory, threshold = 0){
    
    retorno <- numeric()
    completos <- complete(directory)
    superanLimite <- subset(completos, nobs > threshold)[,1]
    
    
    j <- 0
    for(i in superanLimite)
    {
        fileName <- paste (".", "\\" ,directory, "\\" , formatC(i, 2, flag = "0") ,".csv", sep = "")
        partialData <- read.csv(fileName)
        retorno[j] <- cor(partialData[2], partialData[3], use = "complete.obs")
        j <- j + 1
    }
    
    retorno
}