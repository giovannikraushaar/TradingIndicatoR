# Giovanni Kraushaar, 2019-04-26

#' Moving Average Convergence Divergence
#' 
#' Computes the Moving Average Convergence Divergence (MACD), 
#' a trend-following momentum indicator that shows the relationship between
#' two moving averages of a security’s price.
#' 
#' @param price A vector of past prices or an xts object.
#' @param period \code{c(sp,lp)}, a vector containing the length of the short
#' period EMA and the length of the long period EMA. 
#' @param signal_line Boolean, if true the signal line using a period of 
#' \code{slp} is computed.
#' @param slp Signal line period, the length of the period of the EMA used as
#' signal line.
#' @param k Weighting multiplier for the underlying Exponential
#' \code{\link{MA}}, defualt if \code{NULL}.
#' @return A \code{\link[base:vector]{vector}} or an 
#' \code{\link[xts:xts-package]{xts}} object, accordingly to the input, 
#' of the same length of the input. If also the 
#' signal line is computed, a list with two elements: \code{MACD} and 
#' \code{signal}, or a multiple timeseries if it is an xts.
#' @seealso Moving average: \code{\link{MA}}.
#' @author Giovanni Kraushaar <giovanni.kraushaar@usi.ch>
#' @references Murphy John J. (1999),
#' \emph{Technical Analysis of the Financial Markets},
#' New York Institute of Finance.
# #' @importFrom xts xts is.xts merge.xts
#' @export
#' 
#' @examples 
#' # Compute Exponential Moving Average of Bank of America quotes
#' MACD( BAC$Close )
#' 
MACD <- function( price, period = c(12,26), 
                  signal_line = FALSE, slp = 9, k = NULL ){
  
  # input checks
  if (length(period) != 2) {
    stop('Must provide 2 values for period')
  }
  
  
  # MACD
  sp <- min(period)   # short period
  lp <- max(period)   # long  period
  short_ema <- MA( price = price, period = sp, method = 'EMA', k = k )
  long_ema  <- MA( price = price, period = lp, method = 'EMA', k = k )
  macd <- short_ema - long_ema
  
  
  # Signal Line
  if ( signal_line ){
    n  <- length(macd)
    x  <- macd[(lp):n]   # first lp-1 entries are empty
    sl <- MA( x, slp, 'EMA')

    if (xts::is.xts(price)){
      out <- xts::merge.xts(macd,sl)
      colnames(out) <- c('MACD', 'Signal Line')
    } else {
      sl  <- c( rep(NA,lp-1), sl )
      out <- list( MACD = macd, Signal_Line = sl )
    }
    
    return(out)
  }
  
  if (xts::is.xts(price)) colnames(macd) <- 'MACD'
  return( macd )
}

