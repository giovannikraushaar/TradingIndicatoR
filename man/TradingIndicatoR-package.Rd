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
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{TradingIndicatoR}

Maintainer: \packageMaintainer{TradingIndicatoR}
}
\references{
~~ Literature or other references for background information ~~
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
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}

\examples{
p <- c( 20, 22, 24, 25, 23, 26, 28, 26, 29, 27, 28, 30, 27, 29, 28 )
MA(p, method='sma', period = c(10,2))
}
