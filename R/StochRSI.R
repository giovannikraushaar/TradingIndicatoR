# Luca Sanfilippo, 2019-05-09

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
#' @importFrom xts is.xts xts
#' @importFrom zoo coredata index
#' 
#' @author Luca Sanfilippo <luca.sanfilippo@usi.ch>
#' @references Chande Tushard  and Kroll Stanley (1994), 
#' \emph{The New Technical Trader}.
#' 
#' @examples
#' 
#' data(BAC)
#' rsi <- RSI(BAC$Close)
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
  if (class(rsi) == c('numeric') && xts::is.xts(rsi) == F ) {
    dimension <- length(rsi)
    core <- rsi
    ind <- FALSE
  } else if (xts::is.xts(rsi)) {
    dimension <- length(rsi[, 1])
    core <- zoo::coredata(rsi[, ])
    ind <- TRUE
  }
  
  
  # Code to compute the StochRSI
  for (i in period:(dimension - period)) {
    high[i + period - 1]     <- max(rsi[i:(i + period - 1)])
    low[i + period - 1]      <- min(rsi[i:(i + period - 1)])
    stochRSI[i + period - 1] <- 
      (core[(i + period - 1)] - low[i + period - 1]) / 
      (high[i + period - 1] - low[i + period - 1])
  }
  
  if (ind) {
    stochRSI <- 
      xts::xts(c(NA,stochRSI), order.by = zoo::index(rsi[1:dimension]))
  } else {
    stochRSI <- c(NA, stochRSI)
  }
  
  return(stochRSI)
}
