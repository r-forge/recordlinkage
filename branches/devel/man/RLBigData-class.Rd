\name{RLBigData-class}
\Rdversion{1.1}
\docType{class}
\alias{RLBigData-class}


\title{Class "RLBigData"}
\description{
Abstract class for big data sets.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Slots in "RLBigData"}{
  \describe{
    \item{\code{frequencies}:}{Object of class \code{"numeric"}.
      Average frequency of values for each column of the underlying data
      (1 / of number of distinct values)
    }
    \item{\code{blockFld}:}{Object of class \code{"list"}. Blocking definiton.
      See documentation for \link[=RLBigDataDedup]{constructor functions} for details.
    }
    \item{\code{excludeFld}:}{Object of class \code{"numeric"}.
      Indices of attributes which are not considered for comparison.
    }
    \item{\code{strcmpFld}:}{Object of class \code{"numeric"}.
      Indices of attributes on which a string comparator is executed.
    }
    \item{\code{strcmpFun}:}{Object of class \code{"character"}.
      String representing the string comparison function.
    }
    \item{\code{phoneticFld}:}{Object of class \code{"numeric"}.
      Indices of attributes on which a phonetic code is generated.      
    }
    \item{\code{phoneticFun}:}{Object of class \code{"character"}.
      String representing the phonetic coding function.
    }
    \item{\code{drv}:}{Object of class \code{"DBIDriver"}.
      Database driver.
    }
    \item{\code{con}:}{Object of class \code{"DBIConnection"}.
      Database connection.
    }
  }
}


\section{Methods}{
  \describe{
    \item{begin}{\code{signature(x = "RLBigData")}: ... }
    \item{classifySupv}{\code{signature(x = "RecLinkClassif", y = "RLBigData")}: ... }
    \item{classifySupv}{\code{signature(x = "RLBigData", y = "RecLinkClassif")}: ... }
    \item{clear}{\code{signature(x = "RLBigData")}: ... }
    \item{emWeights}{\code{signature(rpairs = "RLBigData", cutoff = "missing")}: ... }
    \item{epiClassify}{\code{signature(rpairs = "RLBigData")}: ... }
    \item{getFrequencies}{\code{signature(x = "RLBigData")}: Get average frequencies of attribute values.}
    \item{getMatchCount}{\code{signature(object = "RLBigData")}: ... }
    \item{getPatternCounts}{\code{signature(x = "RLBigData")}: ... }
    \item{getSQLStatement}{\code{signature(object = "RLBigData")}: ... }
    \item{nextPairs}{\code{signature(x = "RLBigData", n = "numeric")}: ... }
	 }
}
\author{
Andreas Borg
}


\seealso{
  Non-abstract subclasses \code{"\linkS4class{RLBigDataDedup}"} and
  \code{"\linkS4class{RLBigDataLinkage}"} with constructors
  \code{\link{RLBigDataDedup}} and \code{\link{RLBigDataLinkage}}.
}
\examples{
showClass("RLBigData")
}
\keyword{classes}
\keyword{classif}
