\name{TradingIndicatoR-package}
\alias{TradingIndicatoR-package}
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
# Compute Exponential Moving Average of Bank of America quotes
MA( BAC$Close, 50, method = 'EMA' )
}
