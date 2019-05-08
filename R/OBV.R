# Luca Sanfilippo, 2019-04-24

#' On Balance Volume 
#' 
#' Compute the On Balance Volume indicator.    
#' 
#' @param closingPrice A vector of past closing prices or an xts object 
#' @param volume A vector of past volumes or an xts object.
#' 
#' @return A vector or an xts object, accordingly to the input, of the same 
#' length of the input.
#' 
#' @export
#' @author Luca Sanfilippo <luca.sanfilippo@usi.ch>
#' @references \textsc{Joe Granville, 1963}, \emph{New Key To Stock Market Profits}
#' @examples
#' library(quantmod)
#' getSymbols('AAPL',src = 'yahoo')
#' closingPrice <- AAPL$AAPL.Close
#' volume       <- AAPL$AAPL.Volume
#' OBV(closingPrice,volume)
#' plot(OBV(closingPrice,volume)[,1],type = 'l')

OBV <- function (closingPrice, volume) {
  clsCP <- class(closingPrice)
  clsVM <- class (volume)
  
  # Data check
  if (length(closingPrice) < length(volume)) {
    stop(
      paste0(
        'Cannot compute the require OBV with different length inputs \n',
        'closing price length:\t',
        length(closingPrice),
        '\n',
        'volume length: \t',
        length(volume)
      )
    )
  } else if (length(closingPrice) > length(volume)) {
    stop(
      paste0(
        'Cannot compute the require OBV with different length inputs \n',
        'volume length:\t',
        length(volume),
        '\n',
        'closing price length: \t',
        length(closingPrice)
      )
    )
  }
  
  # Code to verify the type of data: (vector, xts)
  if (clsCP == c('numeric') && clsVM == c('numeric')) {
    ind <- FALSE
    obv <- c()
    dimension <- length(closingPrice)
    coreP <- closingPrice
    coreV <- volume
    
  } else if (is.xts(closingPrice) && is.xts(volume)) {
    ind <- TRUE
    obv    <- xts(order.by = index(closingPrice))
    dimension <- length(closingPrice[, 1])
    coreP <- coredata(closingPrice[, 1])
    coreV <- coredata(volume[, 1])
  }
  
  # Code to compute the OBV
  for (i in 2:dimension) {
    obv[1]  <- 0
    if (sum(coreP[i], -coreP[i - 1]) > 0) {
      obv[i] <- obv[i - 1] + coreV[i]
    } else if (sum(coreP[i], -coreP[i - 1]) < 0) {
      obv[i] <- obv[i - 1] - coreV[i]
    } else{
      obv[i] <- obv[i - 1]
    }
  }
  
  if (ind == 'T') {
    obv <- xts(obv, order.by = index(closingPrice[,]))
    obv[1] <- NA
  }else{
    obv[1] <- NA
  }
  
  return(obv)
}
