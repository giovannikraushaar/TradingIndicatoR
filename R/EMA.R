# Giovanni Kraushaar and Paolo Montemurro, 2019-04-23


# Exponential Moving Average

# temporary source
# https://www.fmlabs.com/reference/default.htm?url=ExpMA.htm

EMA <- function( price, period, k ){

  # Vectorization
  if (length(period) > 1){
    x <- lapply(period, function(y) EMA(price=price, period = y, k = k))
    names(x) <- paste0('EMA',period)
    return(x)
  }
  
  if (length(k) > 1){
    x <- lapply(k, function(y) EMA(price=price, period = period, k = y))
    names(x) <- paste0('K',k)
    return(x)
  }
  
  # Data check
  if( length(price) < period ){
    stop( paste0(
      'Cannot compute the require moving average with so few datapoints \n',
      'datapoints:\t', length(price), '\n',
      'period length: \t', period
      ))
  }
  
  # Initialization
  n <- length(price)
  if (is.null(k)) k <- 2 / (period+1)
  x <- rep(NA, n)
  
  # Computation
  x[period] <- mean(price[1:period])
  
  for (t in (period+1):n){
    x[t] <- k * (price[t] - x[t-1]) + x[t-1]
  }
  
  return(x)
}
