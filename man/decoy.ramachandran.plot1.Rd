\name{decoy.ramachandran.plot1}
\alias{decoy.ramachandran.plot1}
\title{
decoy.ramachandran.plot1}
\description{
A function to make the plot with contours and scattered points for the correlation between alpha and tau
}
\usage{decoy.ramachandran.plot1(x.scatter, y.scatter,show,
    x.grid, y.grid , z.grid, plot.title,levels,col)}
\details{
An internal function, to make the ramachandran-like plot with contours and scattered points for alpha and tau.}
\arguments{
\item{x.scatter}{a numeric vector refer the alpha values of the scattered points}
\item{y.scatter}{a numeric vector refer the tau values of the scattered points}
\item{show}{a numeric vector refer the calculated percentages of points fall into the specific contour regions for the specific protein}
\item{x.grid}{a numeric vector as the background grid}
\item{y.grid}{a numeric vector as the background grid}
\item{z.grid}{a numeric density estimation for the contour background}
\item{plot.title}{a character string indicating the title for the ramachandran-like plot, usually "scatter plots over general contours for protein xxxx"}
\item{levels}{percentage levels for the contours in the background}
\item{col}{colors for each level in the contour background, usually }
}
\examples{
id<-"1ABA"
pdb<-read.pdb(id)
ata<-data.ata(id,pdb)
est1<-c()
for (j in 1:nrow(ata)){
est1[j]<-dst1$z[which.min(abs(dst1$x-as.numeric(ata[j,9]))),
which.min(abs(dst1$y-as.numeric(ata[j,10])))]
}
perc1.allowed<-100*length(est1[which(est1>0.00001804)])/length(est1)   #90% level
perc1.favoured<-100*length(est1[which(est1>0.00004661)])/length(est1)  #75% level
perc1.mfav<-100*length(est1[which(est1>0.00009893)])/length(est1)      #50% level
perc1<-round(c(perc1.allowed,perc1.favoured,perc1.mfav),2)
decoy.ramachandran.plot1(as.numeric(ata[,9]), as.numeric(ata[,10]),perc1,
             x.grid=dst1$x, y.grid=dst1$y, z.grid=dst1$z,
             plot.title=paste("scatter plots over general contours for protein",id),
            levels=c(0,0.00001804,0.00004661,0.00009893, 0.001823939),
             col=c('#FFFFFF','#B3E8FF','#7FD9FF','dodgerblue3'))
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal funtion Ramachandran-like plot of alpha-tau}
