\name{clone}
\alias{clone}
\alias{saveRLObject}
\alias{loadRLObject}

\title{
  Serialization of record linkage object.
}
\description{
  These functions saving, loading and deep copying of record linkage objects
  for big data sets.
}
\usage{
  clone(object, ...)
  saveRLObject(object, file = "", inPlace = FALSE, ...)
  loadRLObject(file, inPlave = FALSE)
}
\arguments{
  \item{object}{The object to save.}
  \item{file}{The name of the file to save to or load from.}
  \item{inPlace}{Logical. Wether to store in exisiting file. See details.}
  \item{\dots}{Optional arguments for possible additions, currently not used.}
}
\details{
  As the classes for record linkage of large data sets have a SQLite database
  attached which holds data and generates comparison patterns, a deep copy
  of an instance is not possible by the standard R mechanisms. Also, saving
  and reloading in a different R session makes an object unusable because the
  database connection is lost. These issues are overcome by these
  functions.
  
  \code{clone} makes a deep copy of an object by duplicating the underlying
  database.
  
  \code{saveRLObject} saves an object to a SQLite database file containing
    a representation of the R object as well as the associated database
    tables.

  \code{loadRLObject} loads an object from a file saved by \code{saveRLObject}.
  
  \code{clone} and \code{saveRLObject} are generic functions with methods for
  \code{"\linkS4class{RLBigData}"} and \code{"\linkS4class{RLResult}"}.

  If \code{loadRLObject} is called with \code{inPlace = FALSE} (the default),
  a working copy of the database is made in a temporary file and the original
  file left untouched. To avoid copying (to reduce overhead in the case of
  large files), calling with \code{inPlace = TRUE} sets the provided file as
  working copy and changes made to the database are persistent. Even in this
  case, \code{saveRLObject} should be used to save changes of the R object.
  
  
}
\value{
  \code{clone} returns a deep copy of \code{object}.
    
  \code{loadRLObject} returns the loaded object.

  \code{saveRLObject} is used for its side effects.
}

\section{Side effects}{
  \code{clone} creates a new temporary SQLite database (a copy of the one
    belonging to \code{object}.

  \code{saveRLObject} Writes to the specified file.
}

\note{
  Some methods write to the database and may even create temporary files
  in the same directory (e.g. \code{\link{epiWeights}}), so the file and its
  directory should be writeable.
}
\seealso{\link{clone-methods}, \link{saveRLObject-methods}}


\author{
  Andreas Borg, Murat Sariyar
}

\examples{
  % TODO
}
\keyword{classif}
