\name{RecLinkResult-class}
\Rdversion{1.1}
\docType{class}
\alias{RecLinkResult-class}

\title{Class "RecLinkResult"}
\description{
  S3 class representing information about the classification result of a
  Record Linkage procedure and the data from which this result is derived.
  Registered as a S4 class so that it can appear in method signatures.
}
\section{Objects from the Class}{Object of the S3 class are created by
  classification functions, such as \code{\link{classifySupv}} and 
  \code{\link{emClassify}}. The S4 class is virtual and exists solely 
  for internal usage in method signatures.
}
\section{Slots}{
  The following description refers to the S3 class. An object of class
  \code{"RecLinkResult"} is a list with all mandatory and optional
  items as for \code{"\linkS4class{RecLinkData}"} plus the following:
  \describe{
    \item{\code{prediction}:}{Object of class \code{"factor"}
    Classification of each record, corresponding to the record pairs stored
    in component \code{pairs}. Levels are:
      \describe{
        \item{\code{"L"}}{for links,}
        \item{\code{"P"}}{for possible links,}
        \item{\code{"N"}}{for non-links.}
      }
    }
  }
}
\section{Extends}{
  Class \code{"\linkS4class{RecLinkData}"}, directly.
  Class \code{"\linkS4class{oldClass}"}, by class "RecLinkData", distance 2.
}

\section{Methods}{
  No methods defined with class "RecLinkResult" in the signature.
}

\author{
  Andreas Borg, Murat Sariyar
}


\seealso{
  \code{\link{emClassify}}.
  \code{\link{classifySupv}}.
  \code{\link{RecLinkData}}.
}


\examples{
showClass("RecLinkResult")
}
\keyword{classes}
\keyword{classif}
