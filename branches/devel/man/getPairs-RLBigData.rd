\name{getPairs,RLBigData-method}
\alias{getPairs,RLBigData-method}

\title{
  Retreive record pairs.
}
\description{
A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  \S4method{getPairs}{RLBigData}(object, max.weight = Inf, min.weight = -Inf,
    filter.match = c("match", "unknown", "nonmatch"),
    withWeight = dbExistsTable(rpairs@con, "Wdata"), single.rows = FALSE,
    sort = TRUE)
}

\arguments{
  \item{object}{The object from which to extract pairs.}
  \item{max.weight}{Maximum weight of pairs to include in the output.}
  \item{min.weight}{Minimum weight of pairs to include in the output.}
  \item{filter.match}{Character vector, specifies which pairs appear in the output.}
  \item{withWeight}{Logical. Wether to include weights in the output.}
  \item{single.rows}{Logical. Wether to print record pairs in one row instead
    of two consecutive rows.}
  \item{sort}{Logical. Wether to sort descending by weights.}
}
\details{
  This function extracts record pairs from a \code{"\linkS4class{RLBigDataDedup}"}
  or \code{"\linkS4class{RLBigDataLinkage}"} object. The output can be limited
  to matches, non-matches, pairs with unknown matching status or any combination
  thereof by passing a corresponding character vector as \code{filter.match}.
  
  If weights have been calculated for \code{object} by
  \code{\link[=emWeights,RLBigData-method]{emWeights}} or
  \code{\link[=epiWeights,RLBigData-method]{epiWeights}}, the range of records
  to include in the output can be limited to those with a weight in the interval
  \eqn{[max.weight, min.weight]}. Also, the weight of each record pair will
  be printed in the last column if \code{withWeight} is \code{TRUE} (the default
  if weights are present). Setting \code{sort = TRUE} causes the output to
  be sorted by descending weight.

  If \code{single.rows} is not \code{TRUE}, pairs are output on two consecutive
  lines to allow easy comparison of corresponding attributes. This format is
  mainly intended for printing.
}

\value{
  In the case \code{single.rows == TRUE}, a data frame with the following
  columns:
  \describe{
    \item{\code{id.1}}{ID of the first record (row number in original data
      frame).}
    \item{[...]}{All columns of the first record. \code{".1"} is appended to the
      column name.}
    \item{\code{id.2}}{ID of the first record (row number in original data
      frame).}
    \item{[...]}{All columns of the first record. \code{".2"} is appended to the
      column name.}
    \item{\code{is_match}}{Matching status of the record pair (\code{TRUE}
      for matches, \code{FALSE} for non-matches, \code{NA} for unknown status.}
    \item{\code{Weight}}{Weight of the pair (only for \code{withWeight==TRUE}}
  }
  In the case \code{single.rows == FALSE}, a data frame with the following
  columns:
  \describe{
    \item{\code{id}}{ID of the record.}
    \item{[...]}{All columns of the underlying data.}
    \item{\code{is_match}}{Matching status of the record pairs (see above for
      possible values).}
    \item{\code{Weight}}{Weight of the pair (only for \code{withWeight==TRUE}}
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
