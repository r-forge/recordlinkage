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
  \item{data, train, valid, frequencies,identity}{
    Inherited from \link{RecLinkData}.
  }

  
  \item{prediction}{Logical vector indicating the classification of each record
    pair in \code{valid}
  }
  
  \item{model}{The trained classification model. In the case of rpart, bagging,
    ada and svm this is the predictor returned by the corresponding function, 
    in the case of epilink, the determined threshold.
  }
  
}


\seealso{
\code{\link{classify}}.
\code{\link{RecLinkData}}.
}

\keyword{classif}
