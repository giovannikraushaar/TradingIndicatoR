# Paolo Montemurro, 2019-04-15

#' StochOscillator
#' 
#' Compute stochastic oscillator
#' 
#' @param close vector or xts, historical series of the close price for each interval
#' @param low vector or xts, historical series of the lowest prices for each interval 
#' @param high vector or xts, historical series of the highest prices for each interval 
#' @param n integer, length of each period of analysis
#' @param k integer, length of the smoothing factor
#' 
#' @return vector or xts, historical series of stochastic oscillator value
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references George C. Lane, 
#' \emph{}, 1950.
#' @examples
#' 
#' data(TWTR)
#' StochOscillator(TWTR$Close,TWTR$Low,TWTR$High)
#' data(BAC)
#' StochOscillator(BAC$Close,BAC$Low,BAC$High)
#' 
StochOscillator  <- function(close, high, low, n = 14, k = 3){

  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(close)){
    idx   <- zoo::index(close)
    close <- zoo::coredata(close)
    high <- zoo::coredata(high)
    low <- zoo::coredata(low)
    wasXts<- T
  }
  
  # Check dimension of input
  if(!(length(close) == length(high) & 
       length(close) == length(low))){
    stop("dimension of input is not equal. Forced stop")
    }
  
  values   <- data.frame(matrix(vector(),0,3))

  for(i in n:(length(close))){
    values[i,1] <- close[i]
    values[i,2] <- max(high[i:(i-n)])
    values[i,3] <- min(low[i:(i-n)])
  }
  
  stochOscillator <- (values[,1]-values[,3]) / (values[,2]-values[,3]) * 100
  maStoch <- MA(stochOscillator, period = k)
  
  result <- data.frame(stochOscillator,maStoch)
  colnames(result) <- c("StochOscillator","MA")
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    result <- xts::xts(result, order.by = idx)
  }
    
  return(result)
}

