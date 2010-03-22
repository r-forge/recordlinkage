\name{getPairs}
\alias{getPairs}

\title{Print a Range of Record Pairs}
\description{
  Prints record pairs within a certain weight range for
  threshold assignment.
}
\usage{
getPairs(object, threshold_upper = Inf, threshold_lower = -Inf, 
         single.rows = F, show = "all", sort = T)
}

\arguments{
  \item{object}{A \code{\link{RecLinkResult}} object containing weight information.}
  \item{threshold_upper, threshold_lower}{Real numbers. Upper and lower
    output threshold.}
  \item{single.rows}{Print record pairs in one row instead of two consecutive.}
  \item{show}{Selects which records to show, one of "links", 
    "nonlinks", "possible", "all".}
  \item{sort}{Logical. Whether to sort descending by weight.}
}

\details{
  Input is a \code{\link{RecLinkData}} object for which weights were assigned with 
  \code{\link{emWeights}}. Records are sorted by weight and those for which
  the weigth \code{object$Wdata} holds the condition 
  \code{object$Wdata < threshold_upper && object$Wdata >= threshold_lower}
  are returned. 
  
  Further selection can be made by \code{show} to include all data pairs,
  only links, only non-links or only possible links.
  
  If \code{single.rows} is not \code{TRUE}, pairs are output on two consecutive
  lines to enable easy comparison.
}
\value{
  A data frame. If \code{single.rows} is \code{TRUE}, each row holds (in this
  order) the weight of the data pair, the data fields of the first record and
  the data fields of the second record.

  If \code{single.rows} is not \code{TRUE}, each odd row contains the weight and
  the data fields of the first record and each even row a blank field followed
  by the data fields of the second record.
}
\author{Andreas Borg}

\seealso{\code{\link{RecLinkResult}}}

\keyword{classif}
