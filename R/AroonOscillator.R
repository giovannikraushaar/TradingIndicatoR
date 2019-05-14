# Paolo Montemurro, 2019-04-15

#' Aroon Oscillator
#' 
#' Compute the Aroon Oscillator Indicator
#' 
#' @param price vector or xts, historical series of prices
#' @param n integer, length of each period of analysis
#' 
#' @return vector or xts, historical series of Aroon Oscillator value
#' 
#' @export
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Chande Tushar (1994), \emph{The New Technical Trader: 
#' Boost Your Profit by Plugging Into the Latest Indicators}.
#' @examples 
#' 
#' p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
#' AroonOscillator(p,5)
#' 
AroonOscillator   <- function(price, n = 14){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(price)){
    idx   <- zoo::index(price)
    price <- zoo::coredata(price)
    wasXts<- T
  }
  
  UpDown          <- Aroon(price,n)
  aroonOscillator <- UpDown$AroonUP - UpDown$AroonDOWN
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    aroonOscillator <- xts::xts(aroonOscillator, order.by = idx)
  }
  return(aroonOscillator)
}


