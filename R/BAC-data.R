# Giovanni Kraushaar, 2019-04-24
#
#' Bank of America Stock Price Data
#' 
#' Bank of America Corporation (BAC) historical price time series from 
#' 2013-01-02 to 2018-12-31. Data saved in xts class as downloaded with 
#' quantmod package. Use to test function's compatibility with quantmod
#' output.
#'
#' @docType data
#'
#' @usage data(BAC)
#'
#' @format An object of class \code{"xts"}.
#' 
#' @importFrom xts xts
#' 
#' @keywords datasets, BAC, Bank of America
#'
#' @source \href{https://finance.yahoo.com/quote/BAC?p=BAC}{Yahoo Finance}
#'
#' @examples
#' data(BAC)
#' head(BAC)
"BAC"
#
#
# BAC ------------------------------------------------------------------------
#
# # Data Generating Code
# 
# library(xts)
# library(quantmod)
# 
# getSymbols(Symbols = 'BAC', src='yahoo', auto.assign = TRUE, warnings = FALSE)
# BAC <- BAC['2013-01-01/2018-12-31']
# colnames(BAC) <- c('Open','High','Low','Close','Volume','Adjusted')
# save( BAC, file = 'data/BAC.rda' )
#
# -----------------------------------------------------------------------------