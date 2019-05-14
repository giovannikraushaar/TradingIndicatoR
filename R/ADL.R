# Paolo Montemurro, 2019-04-15

#' ADL
#' 
#' Compute the Accumulation/Distribution Line Indicator
#' 
#' @param close vector or xts, historical series of the close price for each interval
#' @param low vector or xts, historical series of the lowest prices for each interval 
#' @param high vector or xts, historical series of the highest prices for each interval 
#' @param volume vector or xts, historical series of volume for each interval
#' 
#' @return vector or xts, historical series of ADL value
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Taulli Tom (2002), 
#' \emph{The streetsmart guide to short selling: 
#' techniques the pros use to profit in any market}. 
#' McGraw-Hill Professional. p. 190.
#' @examples 
#' 
#' data(TWTR)
#' ADL(TWTR$Close, TWTR$Low, TWTR$High, TWTR$Volume)
#' 
#' data(BAC)
#' ADL(BAC$Close, BAC$Low, BAC$High, BAC$Volume)
#' 
ADL <- function(close, low, high, volume){
  
  # Convert to numeric for easier calculations 
  wasXts  <- F
  if(xts::is.xts(close)){
    idx    <- zoo::index(close)
    close  <- zoo::coredata(close)
    low    <- zoo::coredata(low)
    high   <- zoo::coredata(high)
    volume <- zoo::coredata(volume)
    wasXts<- T
  }
  
  # Check dimension of input
  if(!(length(close) == length(low) & 
       length(close) == length(high) &
       length(close) == length(volume))){
    stop("Dimension of input is not equal. Forced stop")
  }
  
  moneyFlowMultiplier <- NA
  moneyFlowVolume     <- NA
  adl                 <- NA
  moneyFlowMultiplier[1] <- ( (close[1] - low[1]) - (high[1] - close[1]) ) / (high[1] - low[1])
  moneyFlowVolume[1]     <- volume[1] * moneyFlowMultiplier[1]
  adl[1]                 <- moneyFlowVolume[1]
  
  for(k in 2:length(close)){
    moneyFlowMultiplier[k] <- ( (close[k]  -  low[k]) - (high[k] - close[k]) ) / (high[k] - low[k])
    moneyFlowVolume[k]     <- volume[k] * moneyFlowMultiplier[k]
    adl[k]                 <- adl[k-1] + moneyFlowVolume[k]
  }
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    adl <- xts::xts(adl, order.by =idx)
  }
  
  return(adl)
}

