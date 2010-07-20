\name{distbondNoPlots}
\alias{distbondNoPlots}
\title{
distbondNoPlots}
\description{
A function to to get the distribution of bond length between a,b and the probability of x0 for the potential mean values.
}
\usage{distbondNoPlots(a,b,x0)}
\details{
An internal function, to get the potential of a virtual bond length formed by residues a and b.
}
\arguments{
\item{a}{a character string refer the first residue name}
\item{b}{a character string refer the second residue name}

\item{x0}{a numeric value refer the virtual bond length}
}
\value{\item{}{a numeric vector including the potential mean value and the optimal potentials}}
\examples{
library(pressData)
da<-distbondNoPlots("ASN","PRO",3.78)
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal funtion PMF}