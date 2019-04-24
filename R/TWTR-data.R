# Giovanni Kraushaar, 2019-04-24
#
#' Twitter Stock Prices Data
#' 
#' Historical data on daily Twitter, Inc. (TWTR) stock prices between 
#' 2018-04-24 and 2019-04-23 in USD. Retrieved from Yahoo Finance on 
#' April 24th, 2019.
#'
#' @docType data
#' 
#' @details Columns: \code{Date}, \code{Open}, \code{High}, \code{Low}, 
#' \code{Close}, \code{Adj.Close}, \code{Volume}
#'
#' @usage data(TWTR)
#'
#' @format An object of class \code{"data.frame"}.
#'
#' @keywords datasets, twitter, TWTR
#'
#' @source \href{https://finance.yahoo.com/quote/TWTR/history?period1=1524528000&period2=1556064000&interval=1d&filter=history&frequency=1d}{Yahoo Finance}
#'
#' @examples
#' data(TWTR)
#' str(TWTR)
#' head(TWTR)
"TWTR"
#
#
# TWTR ------------------------------------------------------------------------
#
# Data Generating Code
#
# Yahoo Finance made very painful to download the csv files with wget, curl or
# similar because it has embedded session's cookies into the request. Therefore
# I've pre-downloaded the csv file from the browser into the data folder. The
# data, however, can still be manually retrieved from here:
# 'https://finance.yahoo.com/quote/TWTR/history?period1=1524528000&
# period2=1556064000&interval=1d&filter=history&frequency=1d'
# 
# TWTR <- read.csv('data/TWTR.csv')
# TWTR$Date <- as.Date(TWTR$Date)
# save( TWTR, file = 'data/TWTR.rda' )
#
# -----------------------------------------------------------------------------