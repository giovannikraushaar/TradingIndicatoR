# Paolo Montemurro, 2019-04-15

#' Average True Range
#' 
#' Compute the Average True Range Indicator
#' 
#' @param high vector of high prices
#' @param low vector of low prices
#' @param close vector of closing prices
#' @param n integer, length of each period of analysis
#' 
#' @return a vector containing the Average True Range values
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references J. Welles Wilder, 
#' \emph{New Concepts in Technical Analysis Systems}, 1974.
#' @examples
#' 
#' data(BAC)
#' ATR(BAC$High,BAC$Low,BAC$Close)
#' 
ATR <- function(high, low, close, n=14){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(close)){    
    idx   <- zoo::index(close)
    high  <- zoo::coredata(high)
    low   <- zoo::coredata(low)
    close <- zoo::coredata(close)
    wasXts <- T
  }
  
  tr <- NA
  tr[1] <- (high[1]-low[1])
  
  for(i in 2:length(high)){
    tr[i] <- max(c(
      high[i]-low[i], 
      abs(high[i]-close[i-1]), 
      abs(low[i]-close[i-1])
      ))
  }
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    tr <- xts::xts(tr, order.by =idx)
  }
  
  return(MA(tr,period=n))
}

