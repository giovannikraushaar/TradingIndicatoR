# Giovanni Kraushaar, 2019-04-15

SMA <- function(price, period){
  
  # vectorize period input  
  if (length(period) > 1){
    x <- sapply(period, function(y) SMA(price=price, period = y)) 
    names(x) <- paste0('MA',period)
    return(x)
  }
  
  n <- length(price)
  x <- NULL
  for (k in 1:(n-period+1)){
    x[k] <- mean(price[k:(k+period-1)])
  }
  return(x)
}