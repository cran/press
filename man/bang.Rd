\name{bang}
\alias{bang}
\title{bond angle}
\description{
A function for calculating bond angles between 3 connected residues
}
\usage{bang(p1,p2,p3)}
\details{
An internal function, to do calculations of virtual bond angle given the coordinates of three neibourhood residues. 
}
\arguments{
\item{p1}{Coordinate vector of the first residue}
\item{p2}{Coordinate vector of the second residue}
\item{p3}{Coordinate vector of the third residue}}
\examples{
id<-"1ABA"
pdb<-read.pdb(id)
calpha<-pdb$atom[pdb$calpha, c("resid","x","y","z","resno")]
p.calpha<-data.matrix(calpha[,2:4])
## compute the angle formed by 1st, 2nd, and 3rd residues
Ang1<-bang(as.numeric(p.calpha[1,]),as.numeric(p.calpha[2,]),as.numeric(p.calpha[3,]))                     
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal function angle}
