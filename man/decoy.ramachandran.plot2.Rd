\name{decoy.ramachandran.plot2}
\alias{decoy.ramachandran.plot2}
\title{
decoy.ramachandran.plot2}
\description{
A function to make the plot with contours and scattered points for the correlation between beta and tau
}
\usage{decoy.ramachandran.plot2(x.scatter, y.scatter,show,
    x.grid, y.grid , z.grid, plot.title,levels,col)}
\details{
An internal function, to make the ramachandran-like plot with contours and scattered points for beta and tau.}
\arguments{
\item{x.scatter}{a numeric vector refer the alpha values of the scattered points}
\item{y.scatter}{a numeric vector refer the tau values of the scattered points}
\item{show}{a numeric vector refer the calculated percentages of points fall into the specific contour regions for the specific protein}
\item{x.grid}{a numeric vector as the background grid}
\item{y.grid}{a numeric vector as the background grid}
\item{z.grid}{a numeric density estimation for the contour background}
\item{plot.title}{a character string indicating the title for the ramachandran-like plot, usually "scatter plots over general contours for protein xxxx"}
\item{levels}{percentage levels for the contours in the background}
\item{col}{colors for each level in the contour background}
}
\examples{
id<-"1ABA"
pdb<-read.pdb(id)
ata<-data.ata(id,pdb)
est2<-c()
for (j in 1:nrow(ata)){
est2[j]<-dst2$z[which.min(abs(dst2$x-as.numeric(ata[j,11]))),
which.min(abs(dst2$y-as.numeric(ata[j,10])))]
}
perc2.allowed<-100*length(est2[which(est2>0.00001678)])/length(est2)   #90% level
perc2.favoured<-100*length(est2[which(est2>0.00003428)])/length(est2)  #75% level
perc2.mfav<-100*length(est2[which(est2>0.00008791)])/length(est2)      #50% level
perc2<-round(c(perc2.allowed,perc2.favoured,perc2.mfav),2)

decoy.ramachandran.plot2(as.numeric(ata[,11]), as.numeric(ata[,10]),perc2,
             x.grid=dst2$x, y.grid=dst2$y, z.grid=dst2$z,
             plot.title=paste("scatter plots over general contours for protein",id),
            levels=c(0,0.00001678,0.00003428, 0.00008791,0.001822614),
             col=c('#FFFFFF','#C0FFC0','#80FF80','chartreuse3'))
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal funtion Ramachandran-like plot of beta-tau}
