\name{RecLinkClassif-class}
\Rdversion{1.1}
\docType{class}
\alias{RecLinkClassif-class}

\title{Class "RecLinkClassif"}
\description{
  S4 wrapper for S3 class with the same name, which has the same structure
  as a \code{\link{RecLinkData}} object plus a component \code{"prediction"}
  which stores the linkage result.
}
\section{Objects from the Class}{Objects of the S3 class are created by classification
functions, such as \code{\link{classifySupv}} or \code{\link{emClassify}}}
\section{Slots}{
  \describe{
    \item{\code{.S3Class}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{oldClass}"}, directly.
}
\section{Methods}{
  \describe{
    \item{classifySupv}{\code{signature(model = "RecLinkClassif", newdata = "RecLinkData")}: ... }
    \item{classifySupv}{\code{signature(model = "RecLinkClassif", newdata = "RLBigData")}: ... }
	 }
}

\author{
  Andreas Borg, Murat Sariyar
}

\examples{
showClass("RecLinkClassif")
}
\keyword{classes}
