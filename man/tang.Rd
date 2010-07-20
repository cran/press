\name{tang}
\alias{tang}
\title{torsional angle}
\description{
A function for calculating torsional angles between 4 connected residues
}
\usage{tang(p1,p2,p3,p4)}
\details{
An internal function, to do calculations of virtual torsional angle given the coordinates of four neibourhood residues. 
}
\arguments{
\item{p1}{Coordinate vector of the first residue}
\item{p2}{Coordinate vector of the second residue}
\item{p3}{Coordinate vector of the third residue}
\item{p4}{Coordinate vector of the fourth residue}}
\examples{
id<-"1ABA"
pdb<-read.pdb(id)
calpha<-pdb$atom[pdb$calpha, c("resid","x","y","z","resno")]
p.calpha<-data.matrix(calpha[,2:4])
## compute the angle formed by 1st, 2nd, and 3rd residues
Tor1<-tang(as.numeric(p.calpha[1,]),as.numeric(p.calpha[2,]),as.numeric(p.calpha[3,]),as.numeric(p.calpha[4,]))                     
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal function angle}
            
        