# Luca Sanfilippo, 2019-05-09

#' On Balance Volume 
#' 
#' Compute the On Balance Volume indicator.    
#' 
#' @param closingPrice A vector or an xts vector of past closing prices.
#' @param volume A vector or an xts vector of past volumes
#' 
#' @return A vector or an xts object, accordingly to the input, of the same 
#' length of the input.
#' 
#' @export
#' @importFrom xts xts is.xts
#' @importFrom zoo coredata index
#' @author Luca Sanfilippo <luca.sanfilippo@usi.ch>
#' @references Granville Joe (1963), \emph{New Key To Stock Market Profits}.
#' @examples
#' 
#' obv <- OBV(BAC$Close, BAC$Volume)
#' plot( obv, type = 'l')
#' 

OBV <- function (closingPrice, volume) {
  
  clsCP <- class(closingPrice)
  clsVM <- class (volume)
  
  # Data check
  if (length(closingPrice) != length(volume)) {
    stop(
      paste0(
        'Input vectors are not compatible \n',
        'closingPrice length:\t',
        length(closingPrice),
        '\n',
        'volume length: \t',
        length(volume)
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
    
  } else if (xts::is.xts(closingPrice) && xts::is.xts(volume)) {
    ind <- TRUE
    obv    <- xts::xts(order.by = index(closingPrice))
    dimension <- length(closingPrice[, 1])
    coreP <- coredata(closingPrice[, 1])
    coreV <- coredata(volume[, 1])
  }
  
  # Code to compute the OBV
  for (i in 2:dimension) {
    obv[1]  <- 0
    if (sum(coreP[i],-coreP[i - 1]) > 0) {
      obv[i] <- obv[i - 1] + coreV[i]
    } else if (sum(coreP[i],-coreP[i - 1]) < 0) {
      obv[i] <- obv[i - 1] - coreV[i]
    } else{
      obv[i] <- obv[i - 1]
    }
  }
  
  if (ind ) {
    obv <- xts::xts(obv[,1], order.by = index(closingPrice[, ]))
    obv[1] <- NA
  } else{
    obv[1] <- NA
  }
  
  return(obv)
}


