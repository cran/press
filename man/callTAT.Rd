\name{callTAT}
\alias{callTAT}
\title{
callTAT
}
\description{
Opens the dialogue for torsion-angle-torsion correlations. Displays correlations between 1-torsion, bond angle and 2-torsion.
}
\details{
An internal function, usually called by pressing the "TAT - Five Residues" button. Query the database by selecting a series of five residues. Choosing "Any" for one or more of the residues will sum over all possible values of those residues. Choosing "Any" for all four residues will produce the full histogram. Since some sequences of four residues do not occur even once in our databank, it is possible to select a sequence for which there is little or no data. An error message will be produced in these cases. To ensure adequate data, choose "Any" for at least one residue. Either a lattice of 2-D correlations or a 3-D correlation plot can be displayed.  Finally, bin sizes can be changes to make the histograms readable, and there is an option to display either raw frequencies or density on the y-axis.
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal}
