# Paolo Montemurro, 2019-04-15

#' RSI
#' 
#' Compute the Relative Strength Index
#' 
#' @param returns vector of closing prices
#' @param n integer, length of each period of analysis
#' 
#' @return a vector containing RSI value
#' @export
#' @importFrom zoo coredata index
#' @importFrom xts xts is.xts
#' @author Paolo Montemurro <montep@usi.ch>
#' @references Welles Wilder, 
#' \emph{New Concepts in Technical Trading Systems}, 1978
#' @examples
#' 
#' data(BAC)
#' returns <- (BAC$Close / BAC$Open) - 1
#' RSI(returns)
#' 
RSI <- function(returns, n=14){
  
  # Convert to numeric for easier calculations
  wasXts  <- F
  if(xts::is.xts(returns)){
    idx     <- zoo::index(returns)
    returns <- zoo::coredata(returns)
    wasXts<- T
  }
  
  rs             <- c()
  averageGain    <- c()
  averageLoss    <- c()
  averageGain[n] <- mean(returns[1:n]>=0)
  averageLoss[n] <- mean(returns[1:n]<0)
  rs[n]          <- averageGain[n]/averageLoss[n]
  
  for(i in (n+1):length(returns)){
    
    if(returns[i]>0){currentGain <- returns[i]}else{currentGain <- 0}
    if(returns[i]<0){currentLoss <- returns[i]}else{currentLoss <- 0}
    
    subReturns     <- returns[(i-n):i]
    averageGain[i] <- mean(subReturns[subReturns>0])
    averageLoss[i] <- abs(mean(subReturns[subReturns<0]))   
    
    rs[i] <- (
      ( averageGain[i-1]*(n-1) + currentGain)/n ) / 
      (( averageLoss[i-1]*(n-1) + currentLoss)/n )
  }
  
  rsi <- 100 - 100/(1+ rs)
  
  # If input was XTS, reconvert to XTS.
  if(wasXts==T){
    rsi <- xts::xts(rsi, order.by = idx)
  }
  
  return(rsi)
}

