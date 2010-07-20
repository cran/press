\name{read.pdb}
\alias{read.pdb}
\title{ Read PDB File }
\description{
  Read a Protein Data Bank (PDB) coordinate file.
}
\usage{
read.pdb(file, maxlines = 50000, multi = FALSE, rm.insert = FALSE,
         rm.alt = TRUE, het2atom=FALSE, verbose = TRUE)
}
\arguments{
  \item{file}{ a single element character vector containing the name of
    the PDB file to be read, or the four letter PDB identifier for
    online file access. }
  \item{maxlines}{ the maximum number of lines to read before giving up
    with large files. Default is 50,000 lines. }
  \item{multi}{ logical, if TRUE multiple ATOM records are read for all
    models in multi-model files. }
  \item{rm.insert}{ logical, if TRUE PDB insert records are ignored. }
  \item{rm.alt}{ logical, if TRUE PDB alternate records are ignored. }
  \item{het2atom}{ logical, if TRUE HETATM PDB records are stored as
    ATOM records and returned in the output as such, this should be used
    with caution. }  
  \item{verbose}{ print details of the reading process. }
}
\details{
  \code{maxlines} may require increasing for some large multi-model files.
  The preferred means of reading such data is via binary DCD format 
  trajectory files.
}
\value{
  Returns a list of class \code{"pdb"} with the following components:
  \item{atom}{ a character matrix containing all atomic coordinate ATOM data, 
  with a row per ATOM and a column per record type.  See below for details
  of the record type naming convention (useful for accessing columns). }
  \item{het }{ a character matrix containing atomic coordinate records
    for atoms within \dQuote{non-standard} HET groups (see \code{atom}). }
  \item{helix }{ \sQuote{start}, \sQuote{end} and \sQuote{length} of H
    type sse, where start and end are residue numbers \dQuote{resno}. }
  \item{sheet }{ \sQuote{start}, \sQuote{end} and \sQuote{length} of E
    type sse, where start and end are residue numbers \dQuote{resno}. }
  \item{seqres }{ sequence from SEQRES field. }
  \item{xyz }{ a numeric vector of ATOM coordinate data. }
  \item{calpha }{ logical vector with length equal to \code{nrow(atom)}
    with TRUE values indicating a C-alpha \dQuote{elety}. }
}
\references{
  Grant, B.J. et al. (2006) \emph{Bioinformatics} \bold{22}, 2695--2696.
  
  For a description of PDB format (version2.2) see:\cr
  \url{http://www.rcsb.org/pdb/file_formats/pdb/pdbguide2.2/guide2.2_frame.html}.
}
\author{ Barry Grant }
\note{ 
  For both \code{atom} and \code{het} list components the column names can be
  used as a convenient means of data access, namely:
  Atom serial number \dQuote{eleno} ,
  Atom type \dQuote{elety},
  Alternate location indicator \dQuote{alt},
  Residue name \dQuote{resid},
  Chain identifier \dQuote{chain},
  Residue sequence number \dQuote{resno},
  Code for insertion of residues \dQuote{insert},
  Orthogonal coordinates \dQuote{x},
  Orthogonal coordinates \dQuote{y},
  Orthogonal coordinates \dQuote{z},
  Occupancy \dQuote{o}, and
  Temperature factor \dQuote{b}.
  See examples for further details.
}
\keyword{ IO }
