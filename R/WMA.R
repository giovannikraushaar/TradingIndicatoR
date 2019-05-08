# Giovanni Kraushaar, 2019-04-23

# Weighted Moving Average

WMA <- function( price, period, w ){
  
  # Vectorization
  # TODO - to a multiple period input may follow a list of multiple weight
  #        vectors, working accordingly to the period values. Not very user
  #        friendly though.
  if (length(period) > 1){
    x <- lapply(period, function(y) WMA(price=price, period = y, w = NULL))
    names(x) <- paste0('LWMA',period)
    return(x)
  }
  
  # Data input checks and arrangements
  if( !is.atomic(w) ) w <- unlist(w)
  
  if ( is.null(w) ){
    w <- (1:period) / (period*(period+1)/2) 
  }
  
  if( length(w) != period ) {
    stop('The vector of weights provided must have as many elements
         as the time span considered.')
  }
  
  if ( !real_equality(1, sum(w), tol=1e-6) ){
    stop('Weights must sum up to 1.')
  }
  
  if( length(price) < period ){
    stop( paste0(
      'Cannot compute the require moving average with so few datapoints \n',
      'datapoints:\t', length(price), '\n',
      'period length: \t', period
    ))
  }
  
  
  # Computation
  
  n <- length(price)
  x <- rep(NA, period-1)
  y <- sapply((period:n), function(k) sum( price[(k-period+1):k] * w ) )
  z <- c(x,y)
  
  return(z)
}
