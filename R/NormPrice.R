# Paolo Montemurro, 2019-04-15

#' Normalized Price
#' 
#' Compute the normalized price
#' 
#' @param price vector or xts, historical series of prices
#' @param scale integer, scale of the result
#' 
#' @return vector or xts, historical series of normalized prices to the inserted scale
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @examples
#' 
#' p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
#' NormPrice(p,1)
#' 
NormPrice   <- function(price, scale=100){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(price)){
    idx   <- zoo::index(price)
    price <- zoo::coredata(price)
    wasXts<- T
  }
  
  base      <- price[1]
  normPrice <- (price/base)*scale
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    normPrice <- xts::xts(normPrice, order.by =idx)
  }
  
  return(normPrice)
}

