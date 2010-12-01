\name{RLBigDataDedup}
\alias{RLBigDataDedup}
\alias{RLBigDataLinkage}

\title{
  Constructors for big data objects.
}
\description{
  These are constructors which initialize a record linkage setup for
  big datasets, either deduplication of one (\code{RLBigDataDedup})
  or linkage of two datasets (\code{RLBigDataLinkage}).
}

\usage{
RLBigDataDedup(dataset, identity = NA, blockfld = list(), exclude = numeric(0), strcmp = numeric(0), strcmpfun = "jarowinkler", phonetic = numeric(0), phonfun = "pho_h")

RLBigDataLinkage(dataset1, dataset2, identity1 = NA, identity2 = NA, blockfld = list(), exclude = numeric(0), strcmp = numeric(0), strcmpfun = "jarowinkler", phonetic = numeric(0), phonfun = "pho_h")
}
\arguments{
  \item{dataset}{Table of records to be deduplicated. Either a data frame or 
                 a matrix.} 
  \item{dataset1, dataset2}{Two data sets to be linked.}

  \item{identity, identity1, identity2}{Optional vectors (are converted to
              factors) for identifying true matches and
              non-matches. In a deduplication process, two records \code{dataset[i,]}
               and \code{dataset[j,]} are a true match if and only if 
              \code{identity[i,]==identity[j,]}. In a linkage process, two 
              records \code{dataset1[i,]} and \code{dataset2[j,]} are a true 
              match if and only if \code{identity1[i,]==identity2[j,]}.}

  \item{blockfld}{Blocking field definition. A numeric or character
                  vectors or a list of several such vectors,
                  corresponding to column numbers or names. 
                  See details and examples.}

  \item{exclude}{Columns to be excluded. A numeric or character vector
                  corresponding to columns of dataset or dataset1 and dataset2
                  which should be excluded from comparision}

  \item{strcmp}{Determines usage of string comparison. If \code{FALSE}, no
                  string comparison will be used; if \code{TRUE}, string comparison
                  will be used for all columns; if a numeric or character vector 
                  is given, the string comparison will be used for the specified columns.}

  \item{strcmpfun}{Character string representing the string comparison function.
                Possible values are \code{"jarowinkler"} and \code{"levenshtein"}
  }
  
  \item{phonetic}{Determines usage of phonetic code. Used in the same manner as
                  \code{strcmp}}

  \item{phonfun}{Character string representing the phonetic function. Possible
                 values are \code{"pho_h"} and, under certain circumstances,
                 \code{"soundex" (see details).}
  }
}
\details{

  Works as compare.dedup or compare.linkage; record pairs are not created but
  information on how pairs are created stored, comparison patterns are created
  block-wise when classification is performed
  Phonetic function: soundex only available if SQLite (embedded in package RSQLite)
  is compiled with SQLITE_SOUNDEX, which is not the case by default.
}
\value{
  An object of class \code{"\linkS4class{RLBigDataDedup}"} or
  \code{"\linkS4class{RLBigDataLinkage}"}, depending on the called function.
}

\section{Side effects}{
  The RSQLite database driver is initialized via \code{dbDriver("SQLite")}
  and a connection established and stored in the returned object. Extension
  functions for phonetic code and string comparison are loaded into the database.
  The records in \code{dataset} or \code{dataset1} and \code{dataset2} are stored in tables
  \code{"data"} or \code{"data1"} and \code{"data2"}, respectively, and 
  indices are created on all columns involved in blocking.
}

\author{
  Andreas Borg, Murat Sariyar
}


\seealso{
  \code{"\linkS4class{RLBigDataDedup}"}, \code{"\linkS4class{RLBigDataLinkage}"},
  \code{\link{compare.dedup}}, \code{\link{compare.linkage}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classif}

