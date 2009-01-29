\name{compare}
\alias{compare.dedup}
\alias{compare.linkage}
\title{Compare Records}
\description{Builds comparision tables of record pairs for deduplication or
  linkage.}
\usage{

compare.dedup (dataset, blockfld = FALSE, phonetic = FALSE, phonfun = F, 
  strcmp = FALSE, strcmpfun = FALSE, exclude = F, identity = NA, 
  num.non = 0, des.prop = 0.05, adjust = F)

compare.linkage (dataset1, dataset2, blockfld = FALSE, phonetic = FALSE, 
  phonfun = F, strcmp = FALSE, strcmpfun = FALSE, exclude = F, 
  identity1 = NA, identity2 = NA, num.non = 0, des.prop = 0.05, 
  adjust = F)
}
\arguments{
  \item{dataset}{Table of records to be deduplicated. Either a data frame or 
                 a matrix.} 
  \item{dataset1, dataset2}{Two data sets to link.}
  \item{blockfld}{Blocking field definition. A list of integers vectors). 
                  Two records are considered in the output if
                  and only if for one item of \code{blockfield}, the record
                  have equal values in all columns specified by this item.
                  If \code{FALSE}, no blocking will be performed.}
  \item{phonetic}{Determines usage of a phonetic code. If \code{FALSE}, no
                  phonetic code will be used; if \code{TRUE}, the phonetic code
                  will be used for all columns; if a numeric vector is given, the
                  phonetic code will be used for the specified columns.}
  \item{phonfun}{Function for phonetic code.}
  \item{strcmp}{Determines usage of a string metric.}
  \item{strcmpfun}{User-defined function for string metric. Must take as 
                  arguments two character vector of equal length and output
                  a similarity value in the range [0..1].}
  \item{exclude}{Columns to exclude. A numeric vector of indices of columns
                  which should be excluded from comparision. E.g. useful for
                  external identifiers.}                                                                                                            
  \item{identity}{Optional logical vectors for identifying true matches and
              non-matches. In a deduplication process, two records \code{dataset[i,]}
               and \code{dataset[j,]} are a true match if and only if 
              \code{identity[i,]==identity[j,]}. In a linkage process, two 
              records \code{dataset1[i,]} and \code{dataset2[j,]} are a true 
              match if and only if \code{identity1[i,]==identity2[j,]}.}
  \item{num_non}{Number of non-matches to generate for training set. See details
              for information on training sets.}
  \item{des_prop}{Desired proportion of matches to non-matches in generated
            training set.}
  \item{adjust}{Currently not used.}
}                  

\value{An object of class \code{RecLinkPairs} with the following components:
  \item{data}{Copy of the records, converted to a data frame.}
  \item{pairs}{Generated comparision patterns.}
  \item{frequencies}{For each column included in \code{pairs}, the average
    frequency of values (reciprocal of number of distinct values).}   
}

\details{
  These functions group records into record pairs and build comparison patterns
  by which these pairs are later classified as links or non-links. They make up
  the initial stage in an Record Linkage process after possibly 
  normalizing the data. Two general
  scenarios are reflected by the two functions: \code{compare.dedup} works on a
  single data set which is to be deduplicated, \code{compare.linkage} is intended
  for linking two data sets together.
  
  Data sets are represented as data frames or matrices (typically of type 
  character), each row representing one record, each column representing one
  field or attribute (like first name, date of birth\ldots).
  
  Blocking is done by checking identity on an arbitrary number of fields as 
  given by the blockfld argument, while multiple blocking criteria can be 
  combined. Blocking can be omitted, which leads to a large number of record
  pairs (\eqn{\frac{|dataset|-1}{2}}{(length(dataset)-1)/2}).
  
  Fields can be excluded from the linkage process by supplying their column
  index in the vector \code{exclude}. Excluded fields can still be used for
  blocking, also with phonetic code.
  
  Phonetic codes and string similarity measures are supported for better 
  detection of misspelled data. Applying a phonetic code leads to a binary
  comparison value, where 1 denotes equality of the generated phonetic code.
  A string comparator leads to a fuzzy similarity value in the range $[0,1]$.
  String comparison is not allowed on a field for which also a phonetic code
  is generated. Please note that phonetic code and string metrics can slow down
  the generation of comparison patterns significantly.
  
  User-defined functions for phonetic code and string comparison can be supplied
  via the arguments \code{phonfun} and \code{strcmpfun}. \code{phonfun} is 
  expected to have as single argument the string to be transformed, 
  \code{strcmpfun} must have as arguments the two strings to be compared. Both
  functions must be fully vectorized to work on a matrix.
  
  If the arguments \code{identity} (for \code{compare.dedup}) or \code{identity1}
  and \code{identity2} (for \code{compare.linkage} are given, data can be split 
  into training and validation sets. \code{num_non} matches and 
  \code{num_non*des_prop} non-matches are sampled randomly as a training set, 
  all other pairs form the validation set.
  
  For data sets where the true matching status is unknown, training pairs can
  be generated by \code{\link{genSamples}}.
  
}

\seealso{
  \code{\link{RecLinkData}} for the format of returned objects,
  \code{\link{genSamples}} for automatic generation of training data.
}

\author{Andreas Borg}

\keyword{classif}
