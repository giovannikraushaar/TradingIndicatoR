# Paolo Montemurro, 2019-04-15

#' EMV
#' 
#' Compute the Arms Ease of Movement
#' 
#' @param low vector or xts, historical series of the lowest prices for each interval 
#' @param high vector or xts, historical series of the highest prices for each interval 
#' @param volume vector or xts, historical series of volume for each interval
#' @param divisor integer that scales the result
#' 
#' @return vector or xts, historical series of EMV value
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Richard W. Arms, Jr, \emph{Arms Ease of Movement}, 
#' @examples 
#' 
#' data(TWTR)
#' EMV(TWTR$Low, TWTR$High, TWTR$Volume)
#' 
#' data(BAC)
#' EMV(BAC$Low, BAC$High, BAC$Volume)
#' 
EMV  <- function(low, high, volume, divisor = 1e4){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(high)){
    idx    <- zoo::index(high)
    high   <- zoo::coredata(high)
    low    <- zoo::coredata(low)
    volume <- zoo::coredata(volume)
    wasXts<- T
  }
  
  # Check dimension of input
  if(!(length(low) == length(high) & 
       length(low) == length(volume))){
    stop("Dimension of input is not equal. Forced stop")
  }
  
  emv    <- NULL
  emv[1] <- NA
  
  for(n in 2:length(high)){
    emv[n] <- ( ((high[n]+low[n]) / 2) - ((high[n-1]-low[n-1]) / 2) ) / ( (volume[n]/divisor) / (high[n]-low[n]) )
  }
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    emv <- xts::xts(emv, order.by =idx)
  }
  
  return(emv)
}






