# Paolo Montemurro, 2019-04-15

#' Average True Range
#' 
#' Compute the Average True Range Indicator
#' 
#' @param close vector or xts, historical series of the close price for each interval
#' @param low vector or xts, historical series of the lowest prices for each interval 
#' @param high vector or xts, historical series of the highest prices for each interval 
#' @param n integer, length of each period of analysis
#' 
#' @return vector or xts, historical series of ATR value
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Wilder J. Welles (1974), 
#' \emph{New Concepts in Technical Analysis Systems}.
#' @examples
#' 
#' data(BAC)
#' ATR(BAC$Close,BAC$Low,BAC$High)
#' 
ATR <- function(close, low, high, n=14){
  
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

