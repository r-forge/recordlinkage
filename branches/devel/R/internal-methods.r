blockfldfun <- function(blockfld, coln)
{
 paste("(",paste(sapply(blockfld, function(blockvec)
                  paste(sapply(blockvec, function(blockelem)
                    sprintf("t1.%s=t2.%s", coln[blockelem], 
                    coln[blockelem])), collapse=" and ")),
                    collapse=") or ("), ")", sep="")
}

#' Begin generation of data pairs
#'
#' An SQL statement representing the generation of data pairs, including
#' the configuration of blocking fields, phonetics etc. is constructed and
#' send to SQLite.
setGeneric(
  name = "begin",
  def = function(x, ...) standardGeneric("begin")
)
      
setMethod(
  f = "begin",
  signature = "RLBigDataDedup",
  definition = function(x, ...)
  {
    coln <- make.db.names(x@con, colnames(x@data))
    # exclude einbauen
    selectlist_id <- "select t1.row_names as id1, t2.row_names as id2,"
    selectlist <- paste(sapply(coln, 
      function(x) sprintf("t1.%s=t2.%s as %s",x,x,x)), collapse = ", ")
    selectlist <- paste(selectlist, "t1.identity=t2.identity as is_match", sep=",")
    fromclause <- "from data t1, data t2"
    whereclause <- "where t1.row_names < t2.row_names"
    if (length(x@blockFld)>0)
    {
     whereclause <- sprintf("%s and (%s)", whereclause, blockfldfun(x@blockFld, coln))
    }
  # sortieren dürfte zu Performanceproblemen führen
  #  orderclause <- "order by id1, id2")
  
    query <- paste(selectlist_id, selectlist, fromclause, whereclause, collapse=" ")  
    message(query)
    dbSendQuery(x@con, query) # can be retreived via dbListResults(x@con)[[1]]
    return(x)
  }
)

setGeneric(
  name = "nextPairs",
  def = function(x, n=10000, ...) standardGeneric("nextPairs")
)

setMethod(
  f = "nextPairs",
  signature = c("RLBigData", "numeric"),
  definition = function(x, n=10000, ...)
  {
    res <- dbListResults(x@con)[[1]]
    result <- fetch(res, n)
    # Spalten, die nur NA enthalten, werden als character ausgegeben, deshalb
    # Umwandlung nicht-numerischer Spalten in numeric
    if (nrow(result) > 0) # wichtig, weil sonst Fehler bei Zugriff auf Spalten auftritt
    {
      for (i in 1:ncol(result))
      {
        if (!is.numeric(result[,i]))
          result[,i] <- as.numeric(result[,i])
      }
    }
    result
  }
)

setGeneric(
  name = "clear",
  def = function(x, ...) standardGeneric("clear")
)

setMethod(
  f = "clear",
  signature = "RLBigData",
  definition = function(x, ...) dbClearResult(dbListResults(x@con)[[1]])
)


setGeneric(
  name = "getPatternCounts",
  def = function(x, n=10000, ...) standardGeneric("getPatternCounts")
)

setMethod(
  f = "getPatternCounts",
  signature = "RLBigData",
  definition = function(x, n=10000, ...)
  {
   on.exit(clear(x))
   x <- begin(x)
   patternCounts <- 0L
   i = n
   while(nrow(slice <- nextPairs(x, n)) > 0)
   {
    message(i)
    flush.console()
    slice[is.na(slice)] <- 0
    patternCounts <- patternCounts + countpattern(slice[,-c(1,2,ncol(slice))])
     i <- i + n
   }      
   patternCounts
  }
)

