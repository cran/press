\name{distangNoPlots}
\alias{distangNoPlots}
\title{
distangNoPlots}
\description{
A function to to get the distribution of dihedral angle a,b,c and the probability of x0 for the potential mean values.
}
\usage{distangNoPlots(a,b,c,x0)}
\details{
An internal function, to get the potential of a virtual bond angle formed by residues a, b, and c.
}
\arguments{
\item{a}{a character string refer the first residue name}
\item{b}{a character string refer the second residue name}
\item{c}{a character string refer the third residue name}
\item{x0}{a numeric value refer the virtual bond angle}
}
\value{\item{}{a numeric vector including the potential mean value and the optimal potentials}}
\examples{
library(pressData)
da<-distangNoPlots("GLY","ASN","PRO",100.3)
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal funtion PMF}

