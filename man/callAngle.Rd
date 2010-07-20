\name{callAngle}
\alias{callAngle}
\title{
callAngle
}
\description{
Opens the dialogue for bond angle queries. This can display various graphics related to bond angles and 1-3 bond distances.
}
\details{
An internal function, usually called by pressing the "Bond Angle - Three Residues" button. Query the database by selecting a series of three residues. Choosing "Any" for one or more of the residues will sum over all possible values of those residues. Choosing "Any" for all three residues will produce the full histogram. The "Bond Angle Histogram" is the default plot type, but you can also choose a histogram of 1-3 distances, or a correlation between angle and 1-3 distance. Finally, bin sizes can be changes to make the histograms readable, and there is an option to display either raw frequencies or density on the y-axis.
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal}
