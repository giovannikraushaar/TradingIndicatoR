# Luca Sanfilippo, 2019-04-24

#' Stochastic Relative Strenght Index 
#' 
#' Compute the Stochastic Relative Strenght Index indicator.    
#' 
#' @param rsi A vector of the relative strength index values calculated 
#' with a period of 14 days or an xts object.
#' @param period It is a value (in days: usually it is 14days)
#' 
#' @return A vector or an xts object, accordingly to the input, of the same 
#' length of the input.
#' 
#' @export
#' @author Luca Sanfilippo <luca.sanfilippo@usi.ch>
#' @references \textsc{Tushard Chande and Stanley Kroll, 1994}, \emph{The New Technical Trader}
#' @examples
#' getSymbols('AAPL',src = 'yahoo')
#' closingPrice <- AAPL$AAPL.Close
#' rsi <- RSI(closingPrice)
#' StochRSI(rsi)

StochRSI <- function(rsi, period = 14) {
  high     <- c(rep(NA, 2 * (period - 1)))
  low      <- c(rep(NA, 2 * (period - 1)))
  stochRSI <- c(rep(NA, 2 * (period - 1)))
  
  # Data check
  if (length(rsi) < period) {
    stop(
      paste0(
        'Cannot compute the require StochRSI with so few datapoints \n',
        'datapoints:\t',
        length(rsi),
        '\n',
        'period length: \t',
        period
      )
    )
  }
  
  # Code to verify the type of data: (vector, xts)
  if (class(rsi) == c('numeric') && class(rsi) != c("xts", "zoo")) {
    dimension <- length(rsi)
    core <- rsi
    ind <- F
    
  } else if (is.xts(rsi)) {
    dimension <- length(rsi[, 1])
    core <- coredata(rsi[, ])
    ind <- T
  }
  
  # Code to compute the StochRSI
  for (i in period:(dimension - period)) {
    high[i + period - 1]     <- max(rsi[i:(i + period - 1)])
    low[i + period - 1]      <- min(rsi[i:(i + period - 1)])
    stochRSI[i + period - 1] <- (core[(i + period - 1)]
                                 - low[i + period - 1]) / (high[i + period - 1]
                                                           - low[i + period - 1])
  }
  
  if (isTRUE (ind)) {
    stochRSI <- xts(c(NA,stochRSI), order.by = index(rsi[1:dimension]))
  }else{
    stochRSI <- c(NA, stochRSI)
  }
  
  return(stochRSI)
}




 
