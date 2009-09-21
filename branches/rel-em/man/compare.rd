\name{compare}
\alias{compare.dedup}
\alias{compare.linkage}
\title{Compare Records}
\description{Builds comparison patterns of record pairs for deduplication or
  linkage.}
\usage{

compare.dedup (dataset, blockfld = FALSE, phonetic = FALSE, 
  phonfun = pho_h, strcmp = FALSE, strcmpfun = jarowinkler, exclude = FALSE,
  identity = NA, n_match = NA, n_non_match = NA)

compare.linkage (dataset1, dataset2, blockfld = FALSE, 
  phonetic = FALSE, phonfun = pho_h, strcmp = FALSE, 
  strcmpfun = jarowinkler, exclude = FALSE, identity1 = NA, identity2 = NA,
  n_match = NA, n_non_match = NA)
}

\arguments{
  \item{dataset}{Table of records to be deduplicated. Either a data frame or 
                 a matrix.} 
  \item{dataset1, dataset2}{Two data sets to be linked.}
  \item{blockfld}{Blocking field definition. A list of integer vectors
      corresponding to column numbers. 
                  A record pair is included in the output if
                  and only if for one item of \code{blockfld}, the records
                  have equal values in all columns specified by this item.
                  If \code{FALSE}, no blocking will be performed.}
  \item{phonetic}{Determines usage of a phonetic code. If \code{FALSE}, no
                  phonetic code will be used; if \code{TRUE}, the phonetic code
                  will be used for all columns; if a numeric vector is given, the
                  phonetic code will be used for the specified columns.}
  \item{phonfun}{Function for phonetic code. See details.}
  \item{strcmp}{Determines usage of a string metric. See details}
  \item{strcmpfun}{User-defined function for string metric. See details.}
  \item{exclude}{Columns to be excluded. A numeric vector of indices of columns
                  which should be excluded from comparision}                                                                                                            
  \item{identity, identity1, identity2}{Optional numerical vectors for identifying true matches and
              non-matches. In a deduplication process, two records \code{dataset[i,]}
               and \code{dataset[j,]} are a true match if and only if 
              \code{identity[i,]==identity[j,]}. In a linkage process, two 
              records \code{dataset1[i,]} and \code{dataset2[j,]} are a true 
              match if and only if \code{identity1[i,]==identity2[j,]}.}
  \item{n_match, n_non_match}{Number of desired matches and non-matches in
      the result.}
}

\value{An object of class \code{RecLinkPairs} with the following components:
  \item{data}{Copy of the records, converted to a data frame.}
  \item{pairs}{Generated comparision patterns.}
  \item{frequencies}{For each column included in \code{pairs}, the average
    frequency of values (reciprocal of number of distinct values).}   
}

\details{
  These functions build record pairs and comparison patterns
  by which these pairs are later classified as links or non-links. They make up
  the initial stage in a Record Linkage process after possibly 
  normalizing the data. Two general
  scenarios are reflected by the two functions: \code{compare.dedup} works on a
  single data set which is to be deduplicated, \code{compare.linkage} is intended
  for linking two data sets together.
  
  Data sets are represented as data frames or matrices (typically of type 
  character), each row representing one record, each column representing one
  field or attribute (like first name, date of birth\ldots).
  
  Blocking is done by checking identity on an arbitrary number of fields as 
  given by the \code{blockfld} argument, while multiple blocking criteria can be 
  combined. Blocking can be omitted, which leads to a large number of record
  pairs (\eqn{\frac{n(n-1)}{2}}{n*(n-1)/2} where \eqn{n} is the number of
  records.).
  
  As an alternative to blocking, a determined number of \code{n_match} matches 
  and \code{n_non_match} non-matches can be drawn if \code{identity} of
  \code{identity1} and \code{identity2} are supplied. This can be useful for
  generating training sets for the supervised classificators (see 
  \code{\link{trainSupv}}.
  
  Fields can be excluded from the linkage process by supplying their column
  index in the vector \code{exclude}, which is espacially useful for
  external identifiers. Excluded fields can still be used for
  blocking, also with phonetic code.
  
  Phonetic codes and string similarity measures are supported for better 
  detection of misspelled data. Applying a phonetic code leads to a binary
  comparison value, where 1 denotes equality of the generated phonetic code.
  A string comparator leads to a fuzzy similarity value in the range $[0,1]$.
  String comparison is not allowed on a field for which a phonetic code
  is generated. For phonetic encoding functions included in the package, 
  see \link{phonetics}. For the included string comparator, see 
  \code{\link{jarowinkler}}. Please note that phonetic code and string 
  metrics can slow down the generation of comparison patterns significantly.
  
  User-defined functions for phonetic code and string comparison can be supplied
  via the arguments \code{phonfun} and \code{strcmpfun}. \code{phonfun} is 
  expected to have as single argument the string to be transformed, 
  \code{strcmpfun} must have as arguments the two strings to be compared. Both
  functions must be fully vectorized to work on a matrix.
  
  
}


\seealso{
  \code{\link{RecLinkData}} for the format of returned objects,
%-  \code{\link{genSamples}} for automatic generation of training data.
}

\author{Andreas Borg}

\keyword{classif}
