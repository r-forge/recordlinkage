\name{editMatch}
\Rdversion{1.1}
\alias{editMatch}

\title{
  Edit Matching Status
}
\description{
  Allows editing the matching status of record pairs manually.
}
\usage{
editMatch(rpairs)
}

\arguments{
  \item{rpairs}{
    A \code{\link{RecLinkData}} object. The record pairs to edit.
}
}

\details{
  This function brings up an editor (via \code{\link{edit}} where each record
  pair in \code{rpairs} is printed in two consecutive lines, seperated by blank
  lines. The matching status is printed and can be edited in the last column
  following the respective first record. A match is denoted by 1, a non-match
  by 0. \code{NA}s are possible to mark pairs with unkown status. Changes in
  other fields are ignored.
  
  Manual editing of the matching status is useful for clerical review in 
  general, but particularly to label training sets. In conjunction with
  \code{\link{getMinimalTrain}}, good results can be obtained with a manageable
  effort of manual review.
}
\value{
  A copy of \code{rpairs} with edited matching status.
}


\author{
  Andreas Borg
}

\seealso{
  \code{\link{getMinimalTrain}}
}

\keyword{classif}

