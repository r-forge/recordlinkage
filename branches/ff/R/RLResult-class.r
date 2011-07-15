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
    prediction = "ff_vector"
  )#,
#  prototype = prototype(
#    data = NULL,
#    links = matrix(numeric(0), ncol=2, nrow=0),
#    possibleLinks = matrix(numeric(0), ncol=2, nrow=0),
#    nPairs = numeric(0)
#  )
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

# constructs a contengency table of matches
setMethod(
  f = "getTable",
  signature = "RecLinkResult",
  definition = function(object, ...)
  {
    TP=length(which(object$pairs$is_match & object$prediction=="L")) # true positive
    FP=length(which(!object$pairs$is_match & object$prediction=="L")) # false positive
    TN=length(which(!object$pairs$is_match & object$prediction=="N")) # true negative
    FN=length(which(object$pairs$is_match & object$prediction=="N")) # false negative

    tab <- table(as.logical(object$pairs$is_match),object$prediction,
            dnn=list("true status","classification"),useNA="ifany")
    # if "NA" row appears in the table (for pairs with unknown true status),
    # put them in the middle
    if (nrow(tab) == 3)
      tab[c(1,3,2),]
    else
      tab
  }
)

setMethod(
  f = "getTable",
  signature = "RLResult",
  definition = function(object, ...)
  {
    tab <- table.ff(object@data@pairs$is_match, object@prediction,
      useNA = "ifany")
    names(dimnames(tab)) <- c("true status", "classification")
    dimnames(tab)[dimnames(tab)=="1"] <- "TRUE"
    dimnames(tab)[dimnames(tab)=="0"] <- "FALSE"

    tab
  }
)

