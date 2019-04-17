# Giovanni Kraushaar, 2019-04-15

#' Moving Average
#' 
#' @param price A vector of past prices.
#' @param period Integer or vector of integers. Number of datapoints over 
#' which calculate the average.
#' @param method One of \code{c('SMA', 'EMA', 'WMA')}, for simple-, 
#' exponential-, or weighted- moving average.
#' 
#' @export
#' @author Giovanni Kraushaar <giovanni.kraushaar@usi.ch>
#' @examples 
#' # prices vector
#' p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
#'  
#' MA(p, method='sma', period = c(10,2))
MA <- function(price, period, method = 'SMA'){
  
  if (method=='SMA' | method=='sma'){
    return( SMA(price=price, period=period) )
  }
}
