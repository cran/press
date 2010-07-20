\name{callTorsion}
\alias{callTorsion}
\title{
callTorsion
}
\description{
Opens the dialogue for torsion queries. Displays histograms of queried sequences.
}
\details{
An internal function, usually called by pressing the "Torsion, ATA - Four Residues" button. Query the database by selecting a series of four residues. Choosing "Any" for one or more of the residues will sum over all possible values of those residues. Choosing "Any" for all four residues will produce the full histogram. Since some sequences of four residues do not occur even once in our databank, it is possible to select a sequence for which there is little or no data. An error message will be produced in these cases. To ensure adequate data, choose "Any" for at least one residue.  Finally, bin sizes can be changes to make the histograms readable, and there is an option to display either raw frequencies or density on the y-axis.
}
\author{Yuanyuan Huang, Stephen Bonett, and Zhijun Wu}
\keyword{internal}
