# Giovanni Kraushaar, 2019-04-15

#' Moving Average
#' 
#' Computes multiple flavors of the moving average.
#' 
#' @param price A vector of past prices or an equivalent xts object.
#' @param period Integer or vector/list of integers. Number of datapoints over
#' which calculate the average.
#' @param method One of \code{c('SMA', 'EMA', 'WMA')}, for simple-, 
#' exponential-, or weighted- moving average respectively.
#' Shortcurts \code{'s'}, \code{'e'}
#' or \code{'w'} work as well.
#' @param weight The vector of weights to assign to the weighted moving 
#' average. If \code{NULL}, default is Linearly Weighted Moving Average 
#' weights.
#' @param k Weighting multiplier for the EMA. If \code{NULL}, default is
#' \code{2/(n+1)} where \code{n} is the length of the smoothing period. 
#' @return A vector or an \code{\link[xts:xts-package]{xts}} object, 
#' accordingly to the input, of the same length of the input. 
#' The first period-1 values are NA.
#' @author Giovanni Kraushaar <giovanni.kraushaar@usi.ch>
#' @references Murphy John J. (1999),
#' \emph{Technical Analysis of the Financial Markets},
#' New York Institute of Finance.
#' @details 
#' The weighted moving average method (WMA) is a bit trickier to handle because
#' it requires an input (weight) that is functional of the value an other 
#' (period). If no weights are provided, then they are automatically assigned 
#' linearly, \emph{e.g.} \code{1/55, 2/55, ..., 10/55} with 10 periods. 
#' If \code{period} has more than 1 value, then user \code{weight} is ignored 
#' and \emph{Linearly Weighted Moving Average} (\code{LWMA}) is computed.
#' For EMA the parameter \code{period} is the time span over which calculate 
#' the initial point average.
#' 
#' @importFrom zoo index
#' @importFrom xts xts is.xts
#' @export
#' @examples 
#' 
#' # prices vector
#' p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
#' MA(p, method='s', period = c(10,2))
#' 
#' # Compute Exponential Moving Average of Bank of America quotes
#' MA( BAC$Close, 50, method = 'EMA' )
#' 
MA <- function(price, period, method = 'SMA', weight = NULL, k = NULL){
  
  # TODO vecorize method if other inputs are atomic
  
  # Data input checks and arrangements
  if ( !is.numeric_integer(period) ){
    stop('Vector period can contain only integers')
  }
  
  if ( !all(unlist(period)>= 1) ){
    stop('Vector period can contain only strictly positive integers')
  }
  
  # Applying methods
  
  if ( method %in% c('SMA','sma','s') ){
    x <-  SMA(price=price, period=period)
  }
  
  if ( method %in% c('WMA','wma','w') ){
    x <- WMA(price=price, period=period, w=weight)
  }
  
  if ( method %in% c('EMA','ema','e') ){
    x <- EMA(price=price, period=period, k=k)
  }
  
  
  # Make xts compliant
  if (xts::is.xts(price)){
    if (is.list(x)) {
      n <- names(x)
      x <- lapply(x, function(s) xts::xts(s, order.by = zoo::index(price)) )
      names(x) <- n
    } else {
      x <- xts::xts(x, order.by = zoo::index(price))
      colnames(x) <- method
    }
  }
  
  return(x)
}
