\name{summary}
\alias{summary.RecLinkPairs}
\alias{summary.RecLinkResult}
\title{Print summary of Record Linkage data}
\description{Provides statistical information on Record Linkage objects 
             defined in package \code{RecordLinkage}}
\usage{
summary.RecLinkPairs(object,...)

summary.RecLinkResult(object,...)                    
}
\arguments{
  \item{object}{The object for which to print a summary.} 
}                  

\details{The printed information always includes the number of records and record 
       pairs, for RecLinkResult objects also the number of detected matches, 
       non-matches and possible matches. If the true match status for record 
       pairs is given, additional information on such as number of matches and 
       classification errors is given.
       NAs in the classification table stand for unknown matching status
       or possible matches in the prediction.
}

\value{Used for its side effect.}