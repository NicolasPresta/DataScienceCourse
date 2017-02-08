pollutantmean <- function(directory, pollutant, id = 1:332){
    
    for(i in id)
    {
        fileName <- paste (".", "\\" ,directory, "\\" , formatC(i, 2, flag = "0") ,".csv", sep = "")
        partialData <- read.csv(fileName)
        if(exists("dataTotal"))
            dataTotal <- rbind(dataTotal, partialData)
        else
            dataTotal <- partialData
    }
    
    dataTotal <- na.omit(dataTotal)
    
    dataTotal <- dataTotal[,c(pollutant)]
    
    mean(dataTotal)
}


