# Luca Sanfilippo, 2019-05-09

#' Ichimoku Kinko Hyo 
#' 
#' Compute the Ichimoku Kinko Hyo indicator.    
#' 
#' @param high A vector of high prices or an xts object.
#' @param low A vector of low prices or an xts object.
#' @param closingPrice A vector of past closing prices or an xts object.
#' 
#' @return A matrix or an xts object, accordingly to the input, of the same 
#' length of the input.
#' 
#' @export
#' @importFrom xts xts is.xts
#' @importFrom zoo index coredata
#' 
#' @author Luca Sanfilippo <luca.sanfilippo@usi.ch>
#' @references Goichi Hosoda, 1930-69
#' @examples
#' 
#' data(BAC)
#' Ichimoku( BAC$High, BAC$Low, BAC$Close)
#' 

Ichimoku <- function (high, low, closingPrice) {
  
  TenkanSen   <- c(rep(NA, 8))
  KijunSen    <- c(rep(NA, 25))
  SenkouSpanA <- c(rep(NA, 25))
  SenkouSpanB <- c(rep(NA, 51))
  ChikouSpan  <- c()
  clsHigh <- class(high)
  clsLow <- class(low)
  clsCP <- class(closingPrice)
  h <- length(high) 
  l <- length(low)
  a <- length(closingPrice)
  
  if (all(sapply(c(h,l), identical, a)) != T ){
    stop(
      paste0(
        'Cannot compute the require Ichimoku with different length inputs \n',
        'closing price length:\t',
        length(closingPrice),
        '\n',
        'high length: \t',
        length(high),
        '\n',
        'low length: \t',
        length(low)
      )
    )
  }
  
  # Code to verify the type of data: (vector, xts)
  if (clsHigh == c('numeric') &&
      clsLow == c('numeric') &&
      clsCP == c('numeric')) {
    ind <- F
    results     <- c()
    dimensionHigh <- length(high)
    dimensionCP <- length(closingPrice)
    
  } else if (clsHigh == c('xts', 'zoo') &&
             clsLow == c('xts', 'zoo') &&
             clsCP == c('xts', 'zoo')) {
    ind <- T
    results     <- xts(order.by = index(high))
    dimensionHigh <- length(high[,])
    dimensionCP <- length(closingPrice[,])
  }
  
  # over the past 9 periods
  for (i in 1:(dimensionHigh - 8)) {
    TenkanSen[i + 8] <-
      (max(high[i:i + 8]) + max(low[i:i + 8])) / 2
  }
  # over the past 26 periods
  for (i in 1:(dimensionHigh - 25)) {
    KijunSen[i + 25] <-
      (max(high[i:i + 25]) + min(low[i:i + 25])) / 2
  }
  # over the past 26 periods
  for (i in 1:(dimensionHigh - 25)) {
    SenkouSpanA[i + 25] <-
      (TenkanSen[i + 25] + KijunSen[i + 25]) / 2
  }
  # over the past 52 periods
  for (i in 1:(dimensionHigh - 51)) {
    SenkouSpanB[i + 51] <-
      (max(high[i:i + 51]) + min(low[i:i + 51])) / 2
  }
  # current period's closingPrice plotted 26 days back on the chart
  for (i in 26:(dimensionCP)) {
    ChikouSpan[i - 25] <-
      closingPrice[i]
    ChikouSpan[(dimensionCP - 24):dimensionCP] <- 0
  }
  #
  if (!ind) {
    results <-
      matrix(
        c(
          results,
          high,
          low,
          closingPrice,
          TenkanSen,
          KijunSen,
          SenkouSpanA,
          SenkouSpanB,
          ChikouSpan
        ),
        nrow = length(closingPrice)
      )
    
    colnames(results) <-
      c(
        "High",
        "low",
        "Closing Price",
        "TenkanSen",
        "KijunSen",
        "SenkouSpanA",
        "SenkouSpanB",
        "ChikouSpan"
      )
  } else if (ind) {
    results <-
      merge(
        results,
        high,
        low,
        closingPrice,
        TenkanSen,
        KijunSen,
        SenkouSpanA,
        SenkouSpanB,
        ChikouSpan
      )
  }
  return(results)
}
