add2 <- function(x, y){
    x + y
}

adove10 <- function(x){
    use <- x > 10
    x[use]
}

adove <- function(x, n = 10){
  use <- x > n
  x[use]
}

columMean <- function(y, removeNA = TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i], na.rm = removeNA)
  }
  means
}