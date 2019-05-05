# Paolo Montemurro, 2019-04-15

#' BOP
#' 
#' Compute the Balance of Power Indicator
#' 
#' @param close vector of closing prices
#' @param open vector of opening prices
#' @param low vector of low prices
#' @param high vector of high prices
#' 
#' @return A vector containing the BOP value
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Igor Livshin, \emph{Balance of market power}. 
#' @examples 
#' 
#' data(TWTR)
#' BOP(TWTR$Close, TWTR$Open, TWTR$High, TWTR$Low)
#' 
#' data(BAC)
#' BOP(BAC$Close, BAC$Open, BAC$High, BAC$Low)
#' 
BOP <- function(close, open, high, low){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(close)){
    idx    <- zoo::index(close)
    close  <- zoo::coredata(close)
    low    <- zoo::coredata(open)
    high   <- zoo::coredata(high)
    volume <- zoo::coredata(low)
    wasXts<- T
  }
  
  # Check dimension of input
  if(!(length(close) == length(open) & 
       length(close) == length(high) &
       length(close) == length(low))){
    stop("dimension of input is not equal. Forced stop")
  }
  
  bop   <- (close-open)/(high-low)
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    bop <- xts::xts(bop, order.by = idx)
  }
  return(bop)
}

