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
Arms Richard W. Jr. (1971), \emph{Profits In Volume: Equivolume Charting}.

Chande Tushard  and Kroll Stanley (1994), \emph{The New Technical Trader}.

Granville Joe (1963), \emph{New Key To Stock Market Profits}.

Murphy John J. (1999),\emph{Technical Analysis of the Financial Markets}, New York Institute of Finance.

Taulli Tom (2002), \emph{The streetsmart guide to short selling: techniques the pros use to profit in any market}. McGraw-Hill Professional. p. 190.

Wilder J. Welles (1974), \emph{New Concepts in Technical Analysis Systems}.
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
