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
  saveRLObject(object, ...)
  loadRLObject(file)
}
\arguments{
  \item{object}{The object to save.}
  \item{file}{The name of a (writable) file to save the object into.}
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

\seealso{\link{clone-methods}, \link{saveRLObject-methods}}


\author{
  Andreas Borg, Murat Sariyar
}

\examples{
  % TODO
}
\keyword{classif}
