# Giovanni Kraushaar, 2019-05-05

#' Bollinger Bands
#' 
#' Calculates a stock Bollinger Bands based on its volatility.
#' 
#' @param price stock price time serie, either a vector or an xts object
#' @param period smoothing period of the moving average, an integer or a 
#' vector of integers
#' @param n number of standard deviations, an integer or a vector of integers
#' @return a dataframe or an \code{\link[xts:xts-package]{xts}} object 
#' depending on the input, composed of 3 columns: the simple moving average
#' (\code{SMA}), the Upper Bollinger Band value (\code{UpperBB}) and the 
#' Lower Bollinger Band value (\code{LowerBB}). If \code{period} or \code{n}
#' are multivalued then a list contained the output described above for each
#' value.
#' @author Giovanni Kraushaar <giovanni.kraushaar@usi.ch>
#' @references Murphy John J. (1999),
#' \emph{Technical Analysis of the Financial Markets},
#' New York Institute of Finance.
#' @seealso Moving Average function: \code{\link{MA}}.
#' @export
#' @importFrom stats sd
#' @importFrom zoo index
#' @importFrom xts xts is.xts
#' 
#' @examples 
#' 
#' Bollinger( USGDP )
#' 
Bollinger <- function( price, period = 20, n = 2 ){
  
  # vectorize -----
  if (length(period) > 1){
    x <- lapply(period, function(y) Bollinger(price=price, period = y, n = n)) 
    names(x) <- paste0('BB',period)
    return(x)
  }
  
  if (length(n) > 1){
    x <- lapply(n, function(y) Bollinger(price=price, period=period, n=y)) 
    names(x) <- paste0('SD',n)
    return(x)
  }
  # ------
  
  # Check -----
  if( length(price) < period ){
    stop( paste0(
      'Cannot compute Bollinger Bands with so few datapoints \n',
      'datapoints:\t', length(price), '\n',
      'period length: \t', period
    ))
  }
  # -----
  
  
  # Compute -----
  m  <- length(price)
  na <- rep(NA, period-1)
  ma <- sapply( (period:m), function(k) mean(price[(k-period+1):k]))
  ma <- c(na,ma)
  s  <- sapply( (period:m), function(k) sd(price[(k-period+1):k]))
  s  <- c(na,s)
  x  <- data.frame(
    MA      = ma,
    UpperBB = ma + n * s,
    LowerBB = ma - n * s
  )
  # -----
  
  # xts compliant
  if (xts::is.xts(price)){
    x <- xts::xts(x, order.by = zoo::index(price))
  }
  
  return(x)
}