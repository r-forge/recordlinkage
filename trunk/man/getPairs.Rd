\name{getPairs}
\alias{getPairs}

\title{Print a Range of Record Pairs}
\description{
  Prints record pairs within a certain weight range for
  threshold assignment.
}
\usage{
getPairs(rpairs, max.weight = Inf, min.weight = -Inf, 
         single.rows = FALSE, show = "all", sort = TRUE)
}

\arguments{
  \item{rpairs}{A \code{\link{RecLinkResult}} object containing weight information.}
  \item{max.weight, min.weight}{Real numbers. Upper and lower
    output threshold.}
  \item{single.rows}{Logical. Wether to print record pairs in one row instead of two consecutive.}
  \item{show}{Selects which records to show, one of "links", 
    "nonlinks", "possible", "all".}
  \item{sort}{Logical. Whether to sort descending by weight.}
}

\details{
  Input is a \code{\link{RecLinkData}} object for which weights were assigned with 
  \code{\link{emWeights}} or \code{\link{emWeights}}. 
  Records are sorted by weight and those for which
  the weight \code{rpairs$Wdata} holds the condition 
  \code{rpairs$Wdata < max.weight && rpairs$Wdata >= min.weight}
  are returned. 
  
  Further selection can be made by \code{show} to include all data pairs,
  only links, only non-links or only possible links.
  
  If \code{single.rows} is not \code{TRUE}, pairs are output on two consecutive
  lines to enable easy comparison. All data are converted to character, which 
  can lead to a loss of precision. Therefore, two row-format should be used 
  for printing only.
}
\value{
  A data frame. If \code{single.rows} is \code{TRUE}, each row holds (in this
  order) the weight of the data pair, id and data fields of the first record and
  id and data fields of the second record.

  If \code{single.rows} is not \code{TRUE}, each odd row contains the weight
  followed by id and data fields of the first record, each even row a blank 
  field followed by id and data fields of the second record.
}
\author{Andreas Borg}

\seealso{\code{\link{RecLinkResult}}}

\keyword{classif}
