\name{TradingIndicatoR}
\alias{TIR}
\alias{TradingIndicatoR}
\docType{package}
\title{
\packageTitle{TradingIndicatoR}
}
\description{
\packageDescription{TradingIndicatoR}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{TradingIndicatoR}
\packageIndices{TradingIndicatoR}

# Compute Exponential Moving Average of Bank of America quotes
MA( BAC$Close, 50, method = 'EMA' )
}
\author{
\packageAuthor{TradingIndicatoR}

Maintainer: \packageMaintainer{TradingIndicatoR}
}
\references{
Murphy John J.,\emph{Technical Analysis of the Financial Markets}, New York Institute of Finance, 1999.
}

\keyword{ 
  trading, 
  finance, 
  tchnical, 
  analysis,
  indicator,
  financial,
  markets
  }
  
\seealso{
Recommended class for time series: \code{\link[xts:xts-package]{xts}}
}

\examples{
p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
MA(p, method='sma', period = c(10,2))
}
