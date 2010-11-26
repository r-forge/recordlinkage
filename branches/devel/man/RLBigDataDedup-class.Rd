\name{RLBigDataDedup-class}
\Rdversion{1.1}
\docType{class}
\alias{RLBigDataDedup-class}
\alias{getData,RLBigDataDedup-method}

\title{Class "RLBigDataDedup"}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("RLBigDataDedup", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  See also \code{"\linkS4class{RLBigData}"} for inherited slots.
  \describe{
    \item{\code{data}:}{Object of class \code{"data.frame"} Data set. }
    \item{\code{identity}:}{Object of class \code{"factor"} True ID of records in \code{data} }
  }
}
\section{Extends}{
Class \code{"\linkS4class{RLBigData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{getData}{\code{signature(x = "RLBigDataDedup")}: ... }
	 }
	 See also \link{RLBigData-class} for inherited methods.
}
\author{
  Andreas Borg
}


\seealso{
  \code{\link{RLBigDataDedup}}, \link{RLBigData-class}
}
\examples{
showClass("RLBigDataDedup")
}
\keyword{classes}
