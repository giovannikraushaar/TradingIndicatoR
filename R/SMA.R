# Giovanni Kraushaar, 2019-04-15

# Simple Moving Average

SMA <- function(price, period){
  
  # vectorize period input  
  if (length(period) > 1){
    x <- lapply(period, function(y) SMA(price=price, period = y)) 
    names(x) <- paste0('MA',period)
    return(x)
  }
  
  if( length(price) < period ){
    stop( paste0(
      'Cannot compute the required moving average with so few datapoints \n',
      'datapoints:\t', length(price), '\n',
      'period length: \t', period
    ))
  }
  
  n <- length(price)
  x <- rep(NA, period-1)
  y <- sapply( (period:n), function(k) mean(price[(k-period+1):k]))
  z <- c(x,y)
  
  return(z)
}
