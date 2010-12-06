# internal utility function to create blocking definition in SQL
blockfldfun <- function(blockfld, phoneticFld, phoneticFun, coln)
{
  blockElemFun <- function(fldIndex)
  {
    if (fldIndex %in% phoneticFld)
      return(sprintf("%1$s(t1.%2$s)=%1$s(t2.%2$s)", phoneticFun, coln[fldIndex]))
    else 
      return(sprintf("t1.%1$s=t2.%1$s", coln[fldIndex]))
  }
  
 paste("(", paste(sapply(blockfld, function(blockvec)
                  paste(sapply(blockvec, blockElemFun),
                        collapse=" and ")),
                  collapse=") or ("), ")", sep="")
}

#' Create SQL statement
#'
#' Creates SQL statememt to retreive comparison patterns, respecting
#' parameters such as blocking definition and exclusion of fields.
#'
#' @value A list with components "select_list", "from_clause", "where_clause"
#' representing the corresponding parts of the query without the keywords
#' 'SELECT', 'FROM' and 'WHERE'.
setGeneric(
  name = "getSQLStatement",
  def = function(object) standardGeneric("getSQLStatement")
)

setMethod(
  f = "getSQLStatement",
  signature = "RLBigData",
  definition = function(object)
  {
    # constructs select for a single column, to be used by lapply 
    # (see below)
    selectListElem <- function(fldIndex, coln, excludeFld, strcmpFld, strcmpFun,
                              phoneticFld, phoneticFun)
    {
      # nothing if field is excluded
      if (fldIndex %in% excludeFld)
        return(character(0))
        
      # something like 'jarowinkler(t1.fname, t2.fname) as fname'
      if (fldIndex %in% strcmpFld)
        return(sprintf("%1$s(t1.%2$s, t2.%2$s) as %2$s", strcmpFun, coln[fldIndex]))

      # something like 'pho_h(t1.fname)=pho_h(t2.fname) as fname'
      if (fldIndex %in% phoneticFld)
        return(sprintf("%1$s(t1.%2$s)=%1$s(t2.%2$s) as %2$s", phoneticFun, coln[fldIndex]))

      # direct comparison: something like 't1.fname=t2.fname as fname'      
      return(sprintf("t1.%1$s=t2.%1$s as %1$s", coln[fldIndex]))
    }
    coln <- switch(class(object),
      RLBigDataDedup = make.db.names(object@con, colnames(object@data)),
      RLBigDataLinkage = make.db.names(object@con, colnames(object@data1)))
    selectlist_id <- "t1.row_names as id1, t2.row_names as id2"
    # use unlist to delete NULLs from list
    selectlist <- paste(unlist(lapply(1:length(coln), selectListElem,
      coln, object@excludeFld, object@strcmpFld, object@strcmpFun,
      object@phoneticFld, object@phoneticFun)), collapse = ", ")
    selectlist <- paste(selectlist, "t1.identity=t2.identity as is_match", sep=",")
    fromclause <- switch(class(object), RLBigDataDedup = "data t1, data t2",
                                        RLBigDataLinkage = "data1 t1, data2 t2")
    whereclause <- switch(class(object), RLBigDataDedup = "t1.row_names < t2.row_names",
                                        RLBigDataLinkage = "1")
#    if (length(object@excludeFld) > 0)
#      coln <- coln[-object@excludeFld]
    if (length(object@blockFld)>0)
    {
     whereclause <- sprintf("%s and (%s)", whereclause, blockfldfun(object@blockFld,
      object@phoneticFld, object@phoneticFun, coln))
    }
    return(list(select_list = paste(selectlist_id, selectlist, sep=", "),
                from_clause = fromclause, where_clause = whereclause) )
  }
)


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
  signature = "RLBigData",
  definition = function(x, ...)
  {
    sql <- getSQLStatement(x)  
    query <- sprintf("select %s from %s where %s", sql$select_list, 
      sql$from_clause, sql$where_clause)
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

setGeneric(
  name = "getMatchCount",
  def = function(object) standardGeneric("getMatchCount")
)

setMethod(
  f = "getMatchCount",
  signature = "RLBigData",
  definition = function(object)
  {
    sql <- getSQLStatement(object)
    sql_stmt <- sprintf(
      "select count(*) from %s where %s and t1.identity==t2.identity",
      sql$from_clause, sql$where_clause)
    message(sql_stmt)
    return(as.integer(dbGetQuery(object@con, sql_stmt)))
  }
) 


# Get the number of pairs with unknown matching status
setGeneric(
  name = "getNACount",
  def = function(object) standardGeneric("getNACount")
)

setMethod(
  f = "getNACount",
  signature = "RLBigData",
  definition = function(object)
  {
    sql <- getSQLStatement(object)
    sql_stmt <- sprintf(
      "select count(*) from %s where %s and (t1.identity is null or t2.identity is null)",
      sql$from_clause, sql$where_clause)
    message(sql_stmt)
    return(as.integer(dbGetQuery(object@con, sql_stmt)))
  }
)