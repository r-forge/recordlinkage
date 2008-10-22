\name{RecLinkPairs.object}
\alias{RecLinkPairs.object}
\title{
  Record Linkage Data Pairs Object 
}
\description{
  An object representing information about compared records for Record
  Linkage, as returned by function \code{compare}. 
}

\value{
  \item{data}{
    Data frame of original records as given as input to \code{compare}.
  }
  \item{pairs}{
    Data frame of data pairs. Each row represents the comparison of two records, 
    identified by columns \code{id1} and \code{id2}. The following columns contain for each
    considered attribute a real number in the range [0..1] representing the degree of
    similarity. These columns are named according to the respective columns in
    \code{data}.
  }
  \item{frequencies}{Numeric vector with average frequency of values for each column 
    included in \code{pairs} (reciprocal of number of distinct values).
  }
  \item{identity}{Numeric vector to label matching records. Two records 
    \code{data[i,]} and \code{data[j,]} are considered as belonging to the same
    entity if and only if \code{identity[i]==identity[j]}. \code{NA}s can be used for
    records with unknown matching status, these are not used for training
    classifiers.
  }
  
## Removed: can easily be computed from identity
## is_match=identity[pairs$id1]==identity[pairs$id2]  
##  \item{is_match}{Boolean vector representing the true matching status for each
##    item in \code{pairs}, deduced from \code{identity}. Can include \code{NA}s for 
##    pairs with unknown matching status.
##  }
}

\seealso{
\code{\link{compare}}.
}
\keyword{Record Linkage}
\keyword{RecordLinkage}
