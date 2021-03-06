% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RSI.R
\name{RSI}
\alias{RSI}
\title{Relative Strenght Index}
\usage{
RSI(closingPrice, period = 14)
}
\arguments{
\item{closingPrice}{A vector of past closing prices or an xts object or
the column of a dataframe.}

\item{period}{It is a value (in days: usually it is 14days)}
}
\value{
A vector or an xts object, accordingly to the input, of the same 
length of the input.
}
\description{
Compute the Relative Strenght Index indicator.
}
\examples{

data(BAC)
RSI(BAC$Close)

}
\references{
Wilder J. Welles (1974), 
\emph{New Concepts in Technical Analysis Systems}.
}
\author{
Luca Sanfilippo <luca.sanfilippo@usi.ch>
}
