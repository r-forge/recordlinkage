\name{RecLinkData-class}
\Rdversion{1.1}
\docType{class}
\alias{RecLinkData-class}
\alias{RecLinkData.object}
\alias{RecLinkData}
\alias{classifySupv,RecLinkClassif,RecLinkData-method}
\alias{emClassify,RecLinkData-method}
\alias{emWeights,RecLinkData,ANY-method}
\alias{epiClassify,RecLinkData-method}

\title{Class "RecLinkData"}
\description{
  S3 class representing information about record pairs for Record
  Linkage, as returned by functions \code{\link{compare.dedup}} and
  \code{\link{compare.linkage}}. Registered as a S4 class so that it 
  can appear in method signatures.
}
\section{Objects from the Class}{Object of the S3 class are created by
  \code{\link{compare.dedup}} and \code{\link{compare.linkage}}. The S4 class
  is virtual and exists solely for internal usage in method signatures.

\section{Slots}{
  The following description refers to the S3 class. An object of class
  \code{"RecLinkData"} is a list with at least the following items:
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} ~}
    
    \item{\code{pairs}:}{Object of class \code{"data.frame"}
      Data frame of data pairs. Each row represents the comparison pattern of two records, 
      identified by columns \code{id1} and \code{id2}. The other columns contain for each
      considered attribute a real number in the range [0..1] representing the degree of
      similarity. These columns are named according to the respective columns in
      \code{data}. The last column contains the matching status of the pair,
      coded as 1 for a match or 0 for a non-match.
    }
    
    \item{\code{frequencies}:}{Object of class \code{"numeric"} 
      Numeric vector with average frequency of values for each column 
      included in \code{pairs} (reciprocal of number of distinct values).
    }
    
    \item{\code{type}:}{Object of class \code{"character"}
      Identifies whether a linkage
      (\code{"linkage"}) or a deduplication (\code{"deduplication"}) project is 
      represented.}
  }
  The following items are optional:
  \describe{
    \item{\code{M}:}{Object of class \code{"numeric"}
      Vector of m-probabilities as calculated by \code{\link{emWeights}}.
    }
    \item{\code{U}:}{Object of class \code{"numeric"}
      Vector of u-probabilities as calculated by \code{\link{emWeights}}.
    }
    \item{\code{W}:}{Object of class \code{"numeric"}
      Vector of log-likelihood weights as calculated by \code{\link{emWeights}},
      corresponding to binary comparison patterns as created by 
      \code{\link{bincombinations}}.
    }
    \item{\code{W}:}{Object of class \code{"numeric"}
      Vector of log-likelihood weights as calculated by \code{\link{emWeights}},
      corresponding to the rows of \code{pairs}.
    }
    
}
\section{Extends}{
Class \code{"\linkS4class{oldClass}"}, directly.
}
\section{Methods}{
  \describe{
    \item{classifySupv}{\code{signature(x = "RecLinkClassif", y = "RecLinkData")}: ... }
    \item{emClassify}{\code{signature(rpairs = "RecLinkData")}: ... }
    \item{emWeights}{\code{signature(rpairs = "RecLinkData", cutoff = "ANY")}: ... }
    \item{epiClassify}{\code{signature(rpairs = "RecLinkData")}: ... }
	 }
}

\author{
Andreas Borg, Murat Sariyar
}


\seealso{
  \code{\link{compare.dedup}}, \code{\link{compare.linkage}}
  \code{"\linkS4class{RLBigData}"}
}
\examples{
showClass("RecLinkData")
}
\keyword{classes}
\keyword{classif}
