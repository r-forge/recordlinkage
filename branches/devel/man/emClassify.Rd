\name{emClassify}
\alias{emClassify}
\alias{emClassify-methods}
\alias{emClassify,RecLinkData-method}
\alias{emClassify,RLBigData-method}

\title{Weight-based Classification of Data Pairs}
\description{
  Classifies data pairs to which weights were assigned by \code{\link{emWeights}}.
  Based on user-defined thresholds or estimated error rates.
}
\usage{
  emClassify(rpairs, threshold.upper = Inf,
    threshold.lower = threshold.upper, my = Inf, ny = Inf)

  \S4method{emClassify}{RecLinkData}(rpairs, threshold.upper = Inf,
    threshold.lower = threshold.upper, my = Inf, ny = Inf)

  \S4method{emClassify}{RLBigData}(rpairs, threshold.upper = Inf,
    threshold.lower = threshold.upper, my = Inf, ny = Inf)
}

\arguments{
  \item{rpairs}{\code{\link{RecLinkData}} object with weight information.}
  \item{my}{A probability. Error bound for false positives.}
  \item{ny}{A probability. Error bound for false negatives.}
  \item{threshold.upper}{A numeric value. Threshold for links.}
  \item{threshold.lower}{A numeric value. Threshold for possible links.}
}
\details{
  Two general approaches are implemented. The classical procedure
  by Fellegi and Sunter (see references) minimizes the number of
  possible links with given error levels for false links (\code{my}) and
  false non-links (\code{ny}).
  
  The second approach requires thresholds for links and possible links to be set
  by the user. A pair with weight \eqn{w} is classified as a link if 
  \eqn{w\geq \textit{threshold.upper}}{w>=\textit{threshold.upper}}, as a possible link if 
  \eqn{\textit{threshold.upper}\geq w\geq \textit{threshold.lower}}{threshold.upper>=w>=
  treshold.lower} and as a non-link if \eqn{w<\textit{threshold.lower}}{w<threshold.lower}.
  
  If \code{threshold.upper} or \code{threshold.lower} is given, the 
  threshold-based approach is used, otherwise, if one of the error bounds is
  given, the Fellegi-Sunter model. If only \code{my} is supplied, links are
  chosen to meet the error bound and all other pairs are classified as non-links
  (the equivalent case holds if only \code{ny} is specified). If no further arguments
  than \code{rpairs} are given, a single threshold of 0 is used.
  

}

\note{The quality of classification of the Fellegi-Sunter method 
  relies strongly on reasonable estimations of m- and u-probabilities. 
  The results should be evaluated  critically.
}

\value{
  An object containing the classified dataset and the classification result.
  For the \code{"\link{RecLinkData}"} method, a S3 object of class
  \code{"\link{RecLinkResult}"}, for the \code{"\linkS4class{RLBigData}"} method,
  a S4 object of class \code{"\linkS4class{RLResult}"}.
}

\references{Ivan P. Fellegi, Alan B. Sunter: A Theory for Record Linkage,
  in: Journal of the American Statistical Association Vol. 64, No. 328 
  (Dec., 1969), pp. 1183--1210.}

\author{Andreas Borg, Murat Sariyar}

\seealso{\code{\link{getPairs}} to produce output from which thresholds can
  be determined conveniently.}
  
\keyword{classif}

