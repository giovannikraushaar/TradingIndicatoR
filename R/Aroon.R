# Paolo Montemurro, 2019-04-15

#' Aroon
#' 
#' Compute the Aroon indicator
#' 
#' @param price vector or xts, historical series of prices
#' @param n integer, length of each period of analysis
#' 
#' @return dataframe or xts, historical series of AroonUp and AroonDown values
#' 
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Chande Tushar (1994), 
#' \emph{The New Technical Trader: 
#' Boost Your Profit by Plugging Into the Latest Indicators}.
#' @examples 
#'
#' data(TWTR)
#' Aroon(TWTR$Close, 5)
#' 
Aroon  <- function(price,n=14){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(price)){
    idx   <- zoo::index(price)
    price <- zoo::coredata(price)
    wasXts<- T
  }
  
  distance   <- data.frame(matrix(vector(),0,2))
  
  for(i in (n+1):(length(price))){
    distance[i,1] <- (which.max(price[i:(i-n+1)]))
    distance[i,2] <- (which.min(price[i:(i-n+1)]))
  }

  aroon    <- 100*(n-distance)/n
  colnames(aroon) <- c("AroonUP","AroonDOWN")
  
  # If input was XTS, reconvert to XTS.
  if(wasXts){
    aroon <- xts::xts(aroon, order.by =idx)
  }
  
  return(aroon)
  
}

