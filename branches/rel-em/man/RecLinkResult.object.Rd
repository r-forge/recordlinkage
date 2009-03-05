\name{RecLinkResult.object}
\alias{RecLinkResult}
\title{
  Record Linkage Result Object 
}
\description{
  An object representing information about the classification result of a
  Record Linkage procedure. 
}

\value{
  \item{data, pairs, frequencies}{
    Inherited from \link{RecLinkData}.
  }

  
  \item{prediction}{Factor object indicating the classification of each record
    pair in \code{valid}. Levels are:
    \describe{
      \item{"L"}{for links,}
      \item{"P"}{for possible links}
      \item{"N"}{for non-links}
    }
  }
    
}

\author{Andreas Borg}

\seealso{
\code{\link{emClassify}}.
\code{\link{RecLinkData}}.
}

\keyword{classif}
