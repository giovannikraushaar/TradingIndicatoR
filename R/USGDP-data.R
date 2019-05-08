# Giovanni Kraushaar, 2019-04-24
#
#' US Real GDP Data
#' 
#' United States' Real Gross Domestic Product in billions of chained 2012
#' US dollars, seasonally adjusted annual rate, from 1947-01-01 to 2018-10-01
#' with quarterly frequency.
#'
#' @docType data
#'
#' @usage data(USGDP)
#'
#' @format An object of class \code{"xts"}.
#'
#' @keywords datasets, GDP, USGDP
#' 
#' @references U.S. Bureau of Economic Analysis, \emph{Real Gross Domestic 
#' Product} "GDPC1", retrieved from FRED, Federal Reserve Bank of St. Louis; 
#' \code{https://fred.stlouisfed.org/series/GDPC1}, April 24, 2019.
#'
#' @source \href{https://fred.stlouisfed.org/series/GDPC1}{FRED}
#'
#' @examples
#' data(USGDP)
#' head(USGDP)
"USGDP"
#
#
# USGDP ------------------------------------------------------------------------
#
# # Data Generating Code
# 
# library(xts)
# library(quantmod)
# 
# getSymbols(Symbols = 'GDPC1', src='FRED', auto.assign = TRUE, warnings = FALSE)
# USGDP <- xts( coredata(GDPC1), order.by = index(GDPC1) )
# colnames(USGDP) <- 'GDP'
# save( USGDP, file = 'data/USGDP.rda' )
#
# -----------------------------------------------------------------------------