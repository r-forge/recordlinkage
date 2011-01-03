\name{getPairs,RLBigData-method}
\alias{getPairs,RLBigData-method}

\title{
  Retreive record pairs.
}
\description{
A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  \S4method{getPairs}{RLBigData}(object, filter.match = c("match", "unknown", "nonmatch"),
    single.rows = FALSE)
}

\arguments{
  \item{object}{The object from which to extract pairs.}
  \item{filter.match}{Character vector, specifies which pairs appear in the output.}
  \item{single.rows}{Logical. Wether to print record pairs in one row instead
    of two consecutive rows.}
}
\details{
  This function extracts record pairs from a \code{"\linkS4class{RLBigDataDedup}"}
  or \code{"\linkS4class{RLBigDataLinkage}"} object. The output can be limited
  to matches, non-matches, pairs with unknown matching status or any combination
  thereof by passing a corresponding character vector as \code{filter.match}.

  If \code{single.rows} is not \code{TRUE}, pairs are output on two consecutive
  lines to allow easy comparison of corresponding attributes. This format is
  mainly intended for printing.
}

\value{
  In the case \code{single.rows == TRUE}, a data frame with the following
  columns:
  \itemize{
    \item{\code{id.1}: ID of the first record (row number in original data
      frame).}
    \item{All columns of the first record. \code{".1"} is appended to the
      column name.}
    \item{\code{id.2}: ID of the first record (row number in original data
      frame).}
    \item{All columns of the first record. \code{".2"} is appended to the
      column name.}
    \item{\code{is_match} Matching status of the record pair (\code{TRUE}
      for matches, \code{FALSE} for non-matches, \code{NA} for unknown status.}
  }
  In the case \code{single.rows == FALSE}, a data frame with the following
  columns:
  \itemize{
    \item{\code{id}: ID of the record.}
    \item{All columns of the underlying data.}
    \item{\code{is_match}: Matching status of the record pairs (see above for
      possible values).}
  }
  In the latter case, the two record of a pair are output on two consecutive
  rows, seperated by blank rows.
}

\note{

  \itemize{
    \item{Due to conversions in the SQLite database, the column classes of the result
      may (and usually do) differ from the ones in the original data.
    }
    \item{
      When non-matches are included in the output and blocking is permissive,
      the result object can be very large, possibly leading to memory problems.
    }
  }
}
\author{
  Andreas Borg, Murat Sariyar
}
