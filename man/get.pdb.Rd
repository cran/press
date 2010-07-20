\name{get.pdb}
\alias{get.pdb}
\title{ Download PDB Coordinate Files }
\description{
  Downloads PDB coordinate files from the RCSB Protein Data Bank.
}
\usage{
get.pdb(ids, path = "./", URLonly=FALSE)
}
\arguments{
  \item{ids}{ A character vector of one or more 4-letter PDB
    codes/identifiers of the files to be downloaded. }
  \item{path}{ The destination path/directory where files are to be
    written. }
  \item{URLonly}{ logical, if TRUE a character vector containing the URL
    path to the online file is returned and files are not downloaded. If
    FALSE the files are downloaded. }
  
}
\details{
  This is a basic function to automate file download from the PDB.
}
\value{
  Returns a list of successfully downloaded files. Or optionally if URLonly
  is TRUE a list of URLs for said files.
}
\references{
  Grant, B.J. et al. (2006) \emph{Bioinformatics} \bold{22}, 2695--2696.
  
  For a description of PDB format (version2.2) see:\cr
  \url{http://www.rcsb.org/pdb/file_formats/pdb/pdbguide2.2/guide2.2_frame.html}
.
}
\author{ Barry Grant }
\keyword{utilities}
