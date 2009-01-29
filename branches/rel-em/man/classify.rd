\name{classify}
\alias{classify.epilink}
\alias{classify.rpart}
\alias{classify.bagging}
\alias{classify.ada}
\alias{classify.svm}


\title{Record Linkage - supervised classification functions}
\description{Supervised classification of record pairs into links and non-links}

\usage{

  classify.epilink(rpairs,threshold=0)
  
  classify.rpart(rpairs,model=NULL,...)

  classify.bagging(rpairs,model=NULL,...)
  
  classify.ada(rpairs,model=NULL,...)

  classify.svm(rpairs,model=NULL,...)

}

\arguments{
  \item{rpairs}{\link{RecLinkData} object containing the records pairs to
    classify. The component \code{train} must be populated with training data,
    either \link{compare} or \code{gensamples}.
  }                  
}

\value{An object of class \code{RecLinkPairs} with the following components:
  \item{data}{Copy of the records, converted to a data frame.}
  \item{pairs}{Generated comparision patterns.}
  \item{frequencies}{For each column included in \code{pairs}, the average
    frequency of values (reciprocal of number of distinct values).}   
}

\author{Andreas Borg}
\keyword{classif}
