\name{classify}
\alias{classify.em}


\title{Record Linkage - unsupervised classification}
\description{Classifies record pairs into links, non-links and, in the case of
             \code{classify.em}, possible links}

\usage{
  classify.em(rpairs, m=0.97, my=Inf, ny=Inf,...)

  classify.epilink(rpairs)
  
  classify.rpart(rpairs,model=NULL,...)

  classif.

}
\arguments{
  \item{dataset}{Table of records to be compared. Either a data frame or 
                 a matrix.} 
  \item{blockfld}{Blocking field definition. A list of integers (possibly 
                  vectors). Two records are considered in the output if
                  and only if for one item of \code{blockfield}, the record
                  have equal values in the column(s) specified by this item.
                  If \code{FALSE}, no blocking will be performed.}
  \item{phonetic}{Determines usage of a phonetic code. If \code{FALSE}, no
                  phonetic code will be used; if \code{TRUE}, the phonetic code
                  will be used for all columns; if it is a numeric vector, the
                  phonetic code will be used for the specified columns.}
  \item{strcmp}{Determines usage of a string metric. Used in the same way as
                  \code{phonetic}.}
  \item{phonfun}{Function for phonetic code. A user-defined function can be
                  given which outputs a phonetic code for a given character
                  vector.}
  \item{strfun}{User-defined function for string metric. Must take as 
                  arguments two character vector of equal length and output
                  a similarity value in the range [0..1].}
  \item{exclude}{Columns to exclude. A numeric vector of indices of columns
                  which should be excluded from comparision. E.g. useful for
                  external identifiers.}                                                                                                            
}                  

\value{An object of class \code{RecLinkPairs} with the following components:
  \item{data}{Copy of the records, converted to a data frame.}
  \item{pairs}{Generated comparision patterns.}
  \item{frequencies}{For each column included in \code{pairs}, the average
    frequency of values (reciprocal of number of distinct values).}   
}