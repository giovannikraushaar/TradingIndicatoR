# Giovanni Kraushaar, 2019-04-23

# Exponential Moving Average

# temporary source
# https://www.fmlabs.com/reference/default.htm?url=ExpMA.htm

EMA <- function( price, period, k ){
  
  # Alternative 'period-dependent' smoothing
  # # setting k (smoothing factor)
  # if (is.null(alpha)){
  #   k <- 2 / ((1:(length(price)-period))+1)
  # } else if (is.function(alpha)){
  #   k <- alpha( 1:(length(price)-period) )
  # } else if ( is.numeric(alpha) ){
  #   k <- rep( alpha, length(price)-period )
  # } else {
  #   stop('Invalid alpha')
  # }
  
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
  if (is.null(k)) k <- 2 / (n-period+2)
  x <- rep(NA, n)
  
  
  # Computation
  x[period] <- mean(price[1:period])
  
  for (t in (period+1):n){
    x[t] <- k * price[t] + (1-k) * x[t-1]
  }
  
  return(x)
}
