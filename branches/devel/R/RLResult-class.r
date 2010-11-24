#' Large deduplication data set
#'
#' Realization of RLBigData for deduplication of a single data set. Records are
#' stored as rows in \code{data}. Two records \code{data[i,]} and {data[j,]} are 
#' considered equal if and only if \code{identity[i]==identity[j]}
#'
#' @slot data Records to deduplicate
#' @slot identity Identity vector. 
setClass(
  Class = "RLResult",
  representation = representation(
    data = "RLBigData",
    links = "matrix",
    possibleLinks = "matrix",
    nPairs = "numeric"
  )
)

# no constructor, is created by classifying methods

#setMethod(
#  f = "show",
#  signature = "RLResult",
#  definition = function(object)
#  {
#        
#  }
#)
#

setGeneric(
  name = "getTable",
  def = function(object, ...) standardGeneric("getTable")
)

setMethod(
  f = "getTable",
  signature = "RLResult",
  definition = function(object, ...)
  {
    identity1 <- switch(class(object@data),
      RLBigDataDedup = object@data@identity,
      RLBigDataLinkage = object@data@identity1)

    identity2 <- switch(class(object@data),
      RLBigDataDedup = object@data@identity,
      RLBigDataLinkage = object@data@identity2)
    # TP: true positive, FP: false positive, TN: true negative,
    # FN: false negative
    TP <- sum(identity1[object@links[,1]]==identity2[object@links[,2]])
    FP <- sum(identity1[object@links[,1]]!=identity2[object@links[,2]])
    nMatch <- getMatchCount(object@data)
    FN <- nMatch - TP
    TN <- object@nPairs - TP - FN - FP
    tab <- as.table(matrix(c(TN, FN, FP, TP), ncol=2, nrow=2,
      dimnames = list('true status' = c("FALSE", "TRUE"),
                          'classification' = c("N", "L"))))
    tab
  }
)